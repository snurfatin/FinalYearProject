extensions [vid r]

breed [females female]
breed [males male]
breed [homeranges homerange]

; The meaning of all variables, their meaning, and their range of
; possible values are listed in Table 3.3 in main text

globals
[profiler
  all-female-centroids
  cub-females
  cub-males
  breeding-females
  breeding-males
  juvenile-females
  juvenile-males
  transient-males
  transient-females
  litter-size-list
  male-land-tenure-list
  fem-land-tenure-list
  min-prey
  max-prey
  too-far-female
  male-disp-dist-list
  fem-disp-dist-list
  num-infanticide
  total-cub-list
  cub-indep-list
  num-challenges
  dead-male-chall
  dead-fem-starv
  dead-cub-male
  dead-cub-fem
  dead-juv-male
  dead-juv-fem
  dead-tran-male
  dead-tran-fem
  dead-adult-fem
  dead-adult-male
  age-adult-male-died
  age-adult-fem-died
  prey-sum
  total-prey-reduction
  my-seed
  mean-male-territory-size
  mean-female-territory-size
  num-breeding-males
  num-breeding-females
  num-cub-males
  num-cub-females
  num-juv-males
  num-juv-females
  num-transient-males
  num-transient-females
  total-males
  total-females
  num-fem-w-off
  _recording-save-file-name
  region-list
]

males-own
[
  ;state variables:
  age ; Age in months
  dominant-males ; Identities of males that have beaten this male in challenges
  females-in-my-territory ; agent-set of females within male's territory
  initial-male? ; Indicates whether male was created at beginning of simulation
  lost-territory? ; Indicates if male lost a territory to a challenger
  male-land-tenure ; Total time that male has held onto territory
  my-mom ; the identity of cub's mom
  natal-origin ; Patch where male was initialized at or the centroid patch of mother's territory
  age-class ; Indicates age class of male (i.e., cub, juvenile, transient, or breeder)
  territory ; Set of patches belonging to territory

  ;auxiliary variables:
  age-years ; Age in years
  mcp ; Size in square kilometers of minimum convex polygon surrounding territory
  meanXY ; Mean value of all XY coordinates of patches belonging to territory
  my-fem-centroids ; Centroids of female territories within male territory
  name ; ID used when drawing minimum convex polygons around territory
  nearby-females ; Females within radius
  X ; List of X coordinates of patches belonging to territory
  XY ; List of XY coordinates of patches belonging to territory
  Y ; List of Y coordinates of patches belonging to territory
  initial-females ; records # of females at beginning of time step. This is used to determine if new breeding male has added females to territory
  male-disp-dist ; if new breeding male has added females to his terrritory, then we measure the dispersal distance from his natal range to his post-natal range.
]

females-own
[
  ;state variables:
  age ; Age in months
  fertile? ; Indicates whether female is fertile
  gestating? ; Indicates whether female is gestating
  male-in-my-territory ; agent-set of male with territory overlapping female
  my-mom ; the identity of cub's mom
  my-offspring ; Number of offspring in current litter
  natal-origin ; Patch where female was initialized at or the centroid patch of mother's territory
  fem-land-tenure ; Total time that female has held onto territory
  num-litters ; Total number of litters the female has had up until current time
  age-class ; Indicates age class of female (i.e., cub, juvenile, transient, or breeder)
  territory ; Set of patches belonging to territory
  terr-orig ; Patch that female was initialized at or first patch of territory
  t-gestation ; Indicates how long female has gestated
  t-parenting ; Indicates how long female has been a parent of current litter
  total-cubs ; records total number of cubs that female has ever had

  ;auxiliary variables:
  age-years ; Age in years
  food-initial ; Total food in territory at beginning of time step
  mcp ; Size in square kilometers of minimum convex polygon surrounding territory
  meanXY ; Mean value of all XY coordinates of patches belonging to territory
  name ; ID used when drawing minimum convex polygons around territory
  total-food ; Total food in territory at end of time step
  tot-food-change ; Change in food in territory from beginning to end of time step
  X ; List of X coordinates of patches belonging to territory
  Y ; List of Y coordinates of patches beloning to territory
  XY ; List of XY coordinates of patches belonging to territory
  fem-disp-dist ; if new breeding female has established a territory, then we measure the dispersal distance from her natal range to her post-natal range.
  cub-to-indep ; records number of cubs that became transient (i.e., independent from mom)
]

patches-own
[
  ;state variables:
  owner-fem ; the identity of the female "owning" a patch (i.e., belongs to her territory or home range)
  owner-male ; the identity of the male "owning" a patch (i.e., belongs to his territory or home range)
  prey ; Prey produced at patch
  region ; The region of landscape where the patch belongs to

  ;auxiliary variables:
  cluster ; The cluster of patches within the territory the patch belongs to
  cluster-id ; Number assigned to patch within cluster to determine cluster size
  owner-fem-centroid ; The female for which the patch is her territory centroid
]

homeranges-own
[
  name
]


;-------------------------------------------------------------------------------------------------------------------------------------------

to setup
  clear-all
  r:clear
  r:eval ".libPaths(c('C:/Users/snurf/AppData/Local/R/win-library/4.2',.libPaths()))"
  r:eval "library(adehabitatHR)"
  r:eval "library(sp)"
  set my-seed new-seed
  random-seed my-seed ; here you can control randomness - make debugging easier: the program always will do the same with this
                      ; If you want to see a different sequence, use a different seed (or put an input field on the interface).
                      ;print (word "the seed:" my-seed) ; You can use this print command for debugging.
                      ;In case you see a bug, you can go back and run the model using the same seed from before

  ; Record the minimum and maximum prey production value
  ifelse prey-biomass = "homogeneous"
  [
    set min-prey 0
    set max-prey 4.84
  ]
  [
    set min-prey 2.05
    set max-prey 10.46
  ]

  ; Set the initial value of prey of each patch
  ask patches
  [
    set owner-fem nobody
    set owner-male nobody
    set cluster nobody
    set owner-fem-centroid nobody
    (
      ifelse
      prey-biomass = "homogeneous" [
        set prey min-prey
      ]

      prey-biomass = "random" or prey-biomass = "sm_random" [
        set prey min-prey + (random-float (max-prey - min-prey))
        if (max-prey - prey) < 2 [ set prey prey - 2 ]
      ]

      prey-biomass = "lr_gradient" [
        let m (max-prey - min-prey) / (max-pxcor - min-pxcor)
        set prey m * pxcor + min-prey
      ]
    )
  ]

  ; Spread the value of prey of all patches to their eight surrounding patches to create slight gradient
  if prey-biomass = "sm_random" [
    repeat 5 [ diffuse prey 1 ]
  ]

  ; Divide the patches into a number of region(s) corresponds to num-of-fem
  set region-list ["r1" "r2" "r3" "r4"]

  if prey-biomass = "homogeneous" or prey-biomass = "random" or prey-biomass = "sm_random" [
    (
      ifelse
      num-of-fem = 1 [
        ask patches [ set region item 0 region-list ]
      ]

      num-of-fem = 2 [
        ask patches [
          ifelse pxcor <= max-pxcor / 2 [ set region item 0 region-list ] [ set region item 1 region-list ]
        ]
      ]

      num-of-fem = 3 [
        ask patches [
          (
            ifelse
            pxcor <= max-pxcor / 3 [ set region item 0 region-list ]
            pxcor >= max-pxcor / 3 and pxcor <= 2 * max-pxcor / 3 [ set region item 1 region-list ]
            pxcor >= 2 * max-pxcor / 3 [ set region item 2 region-list ]
          )
        ]
      ]

      num-of-fem = 4 [
        ask patches [
          (
            ifelse
            pxcor <= max-pxcor / 2 and pycor <= max-pycor / 2 [ set region item 0 region-list ]
            pxcor <= max-pxcor / 2 and pycor >= max-pycor / 2 [ set region item 1 region-list ]
            pxcor >= max-pxcor / 2 and pycor <= max-pycor / 2 [ set region item 2 region-list ]
            pxcor >= max-pxcor / 2 and pycor >= max-pycor / 2 [ set region item 3 region-list ]
          )
        ]
      ]
    )
  ]

  ; Update the prey value of some patches to create cluster(s) of patches with the highest prey value possible (max-prey)
  let i 0
  while [ i < num-of-fem ]
  [
    (
      ifelse
      prey-biomass = "homogeneous" [
        ask one-of patches with [ region = item i region-list ]
        [
          ask patches in-radius 4 [ set prey max-prey ]
        ]
        repeat 15 [ ask one-of patches with [ prey = min-prey and (count neighbors4 with [prey = max-prey and region = item i region-list]) = 1 ]
          [
            set prey max-prey
          ]
        ]
        repeat 50 [ ask one-of patches with [ prey = min-prey and (count neighbors4 with [prey = max-prey and region = item i region-list]) >= 1 ]
          [
            set prey max-prey
          ]
        ]
      ]

      prey-biomass = "random" or prey-biomass = "sm_random" [
        ask one-of patches with [ region = item i region-list ]
        [
          set prey max-prey
        ]
        repeat 100 [
          ask one-of patches with [ prey < max-prey and (count neighbors4 with [prey = max-prey and region = item i region-list]) = 1 ]
          [
            set prey max-prey
          ]
        ]
        repeat 10 [
          ask one-of patches with [ prey = max-prey and (count neighbors4 with [prey < max-prey and region = item i region-list]) > 2 ]
          [
            ask one-of neighbors4 [ set prey max-prey ]
          ]
        ]
      ]

      prey-biomass = "sm_random" [
        ask patches with [ prey < max-prey and (count neighbors4 with [prey = max-prey and region = item i region-list]) = 4 ]
        [
          set prey max-prey
        ]
      ]
    )
    set i i + 1
  ]

  ; Update the prey value of some rightmost patches with the highest value possible (max-prey)
  if prey-biomass = "lr_gradient"
  [
    ask patches with [ pxcor >= max-pxcor - (2 * num-of-fem) ]
    [
      set prey max-prey
    ]
    ask n-of (8 * num-of-fem) patches with [ pxcor = max-pxcor - (2 * num-of-fem + 1) ]
    [
      set prey max-prey
    ]
  ]

  ; Color the patches a shade of black proportional to the value of prey
  ask patches
  [
    set pcolor scale-color black prey min-prey max-prey
  ]

  ; Females are created and distributed in different parts of the model landscape
  create-females num-of-fem
  [
    move-to one-of patches with [prey = max-prey and not any? other females in-radius 12]
    set age 36 + random (180 - 84)
    set age-years (floor (age / 12) + 1)
    set age-class "breeder"
    set shape "square"
    set terr-orig patch-here
    set territory patch-set terr-orig    ; the initial territory consists of only one patch
    set size 5
    set name (word "t" who) ; $name
    set t-parenting random 22            ; This indicates female is parent (and can't have litter currently) though she is not technically parenting.
                                         ; This ensures that females do not all have litters at the same time and at the first time step. Once t-parenting reaches 24 then female becomes fertile
                                         ; and able to mate and have litter.
    set color orange ;violet
    set fertile? false
    set gestating? false
    if view-terr?
    [
      set pcolor (color - 2)
    ]
    set owner-fem self                   ; owner-fem is a variable of the patch the female is located on
    set owner-fem-centroid self
    set male-in-my-territory no-turtles
    set my-offspring no-turtles
    set my-mom 1 + random 1000 ; generates random value for my-mom because the initial females do not have my-mom set.
                               ; My-mom must be set to non-zero value for "male-select-location" to work properly
  ]

  ; Males are created and distributed in different parts of the model landscape
  create-males 1
  [
    move-to one-of patches with [prey > 0 and not any? other males in-radius 20]
    set age 36 + random (180 - 84)
    set age-years (floor (age / 12) + 1)
    set age-class "breeder"
    set shape "triangle"
    set size 6
    set name (word "t" who) ; $name
    set color blue ;red;
    set territory no-patches
    set females-in-my-territory no-turtles
    set nearby-females no-patches
    set dominant-males no-turtles
    set natal-origin patch-here
    set initial-male? true
    set my-mom 1 + random 1000 ; generates random value for my-mom because the initial females do not have my-mom set.
                               ; My-mom must be set to non-zero value for "male-select-location" to work properly
  ]

  ; Assign tigers to different stages (remember: in NetLogo agent-sets include pointers to agents, which
  ; means that when a tiger dies, it is automatically removed from all agent-sets it is part of. However,
  ; assignments are not updated automatically with, e.g., age, this will be updated in the procedure "update-age-stage-class"):
  set cub-males males with [age < 12] ; agent-set of male cubs
  set cub-females females with [age < 12] ; agent-set of female cubs
  set juvenile-males males with [age >= 12 and age < 24] ; agent-set of male juveniles
  set juvenile-females females with [age >= 12 and age < 24] ; agent-set of female juveniles
  set transient-males males with [age >= 24 and age < 36] ; agent-set of male transients
  set transient-females females with [age >= 24 and age < 36] ; agent-set of female transients
  set breeding-males males with [age >= 36] ; agent-set of male breeders
  set breeding-females females with [age >= 36] ; agent-set of female breeders
  set all-female-centroids patches with [any? females-here] ; this allows males to find the females in the first time step
  if lists? = true
  [
    set age-adult-male-died []
    set age-adult-fem-died []
    set litter-size-list []
    set male-land-tenure-list []
    set fem-land-tenure-list []
    set male-disp-dist-list []
    set fem-disp-dist-list []
    set total-cub-list []
    set cub-indep-list []
  ]

  reset-ticks
end ; setup

;-------------------------------------------------------------------------------------------------------------------------------------------

to go
  if ticks = 12 [stop] ; Simulation for a year
  ;  if ticks = 680 [stop]
  if count males = 0 [print (word "NO MALES!")
    stop]
  if count females = 0 [print (word "NO FEMALES!")
    stop]
  set dead-cub-male 0
  set dead-cub-fem 0
  set dead-juv-male 0
  set dead-juv-fem 0
  set dead-tran-male 0
  set dead-tran-fem 0
  set dead-adult-male 0
  set dead-adult-fem 0
  set num-infanticide 0
  set num-challenges 0
  set dead-male-chall 0
  set dead-fem-starv 0
  ask patches [set cluster-id ""]

  ; In the first four years of simulation, no natural mortality is applied so that a quasistationary distribution of
  ; territories is reached faster. The first 200 simulations are always ignored for model evaluation:
;  if ticks > 48 [mortality]

  update-age-stage-class  ; Age and stage are updated.

  ask females with [age = 36] [female-select-location] ; Females that have just reached breeding age will select a location for their territory
  ask males with [age >= 36 and count females-in-my-territory = 0] ; Males that have just reached breeding age or who have been unable to win females will select a location for his territory
    [male-select-location]

  update-female-territory ; The initial location of a female is her territory origin. This process expands her territory

  ; In the first four years of simulation, no mortality from starvation is applied so that a quasistationary distribution of
  ; territories is reached faster:
;  if ticks > 48 [female-starvation]
  calculate-fem-centroid  ; The centroid is used to determine which female territories are within the range of a male.
  establish-or-update-male-territory ; Unlike females, males can start this process with no territory but potentially add females to their territory here.
                                     ; Males can also add more females to their existing territory.
  calculate-male-centroid ; These centroids are used by males to determine how close female territories are to his own

;  parenting   ; Updates the time since a female gave birth.
;  gestation   ; Updates gestation time; when its over, females reproduce.
;  prob-mating ; Determines the probability that female will give birth if she if fertile and male overlaps her territory

  diagnostics ; Some debug code.
;  update-plots-and-outputs
  if draw-MCP = true and ticks > 0 [calc-homerange] ; Can add this for visualization
  tick
end ; go

;-------------------------------------------------------------------------------------------------------------------------------------------

to mortality

  ; mortality process for resident females and males that have an established territory
  ; mortality probabilities derived from Kenney et al. 1995 and 2014:
  ask females with [age >= 180]
  [

    ; the max age set to 180 months after which female dies:
    ; return the dead female's territory to default conditions:
    ask territory
      [
        set cluster nobody
        set owner-fem nobody
        set owner-male nobody
        set cluster-id ""
        set pcolor scale-color black prey min-prey max-prey
    ]

    ; tell males to remove this female from their territories:
    let old-dead-female self
    if count male-in-my-territory > 0
    [
      ask male-in-my-territory
      [
        set females-in-my-territory females-in-my-territory with [self != old-dead-female]
        if count females-in-my-territory = 0
          [
            set territory no-patches ; This is the territory of a male.
            set lost-territory? true ; male has completely lost his territory, now making him a floater looking for unoccupied females
        ]
      ]
    ]
    set dead-adult-fem dead-adult-fem + 1
    if lists? = true and ticks >= 200 and fem-land-tenure > 0
    [
      set age-adult-fem-died lput age age-adult-fem-died
      set fem-land-tenure-list lput fem-land-tenure fem-land-tenure-list
      set total-cub-list lput total-cubs total-cub-list
      set cub-indep-list lput cub-to-indep cub-indep-list
    ]
    ; have offspring belonging to dead female die as well:
    if count my-offspring > 0
      [
        ask my-offspring
          [
            ifelse breed = males
            [
              if age-class = "cub"
              [
                set dead-cub-male dead-cub-male + 1
                ;print (word "MALE CUB "who" JUST DIED BECAUSE HIS MOM DIED!")
              ]
              if age-class = "juvenile"
              [
                set dead-juv-male dead-juv-male + 1
                ;print (word "MALE JUVENILE "who" JUST DIED BECAUSE HIS MOM DIED!")
              ]
            ]
            [
              if age-class = "cub"
                [
                  set dead-cub-fem dead-cub-fem + 1
                  ;print (word "FEMALE CUB "who" JUST DIED BECAUSE HER MOM DIED!")
              ]
              if age-class = "juvenile"
                [
                  set dead-juv-fem dead-juv-fem + 1
                  ;print (word "FEMALE JUVENILE "who" JUST DIED BECAUSE HER MOM DIED!")
              ]
            ]
            die
        ]
    ]
    die ; Old female dies.
    if count females = 0 [print (word "NO FEMALES!")
      stop]
  ]
  ask females with [age >= 36 and count territory > 0]
    [
      let m random-float 1
      let res-fem-surv-year breed-fem-surv ; 0.95
      let res-fem-surv-month res-fem-surv-year ^ (1 / 12)

      if m < (1 - res-fem-surv-month) ; monthly mortality
      [

        ; This female is going to die, but first its territory patches and its male have
        ; to be updated.

        ; Return the dead female's territory to default conditions:
        ask territory
        [
          set cluster nobody
          set owner-fem nobody
          set owner-male nobody
          set cluster-id ""
          set pcolor scale-color black prey min-prey max-prey
        ]

        ; Tell males to remove this female from their territories:
        let dead-female self
        if count male-in-my-territory > 0
        [
          ask male-in-my-territory
          [
            set females-in-my-territory females-in-my-territory with [self != dead-female]

            ; If the male has no females then his territory is erased as well:
            if count females-in-my-territory = 0
            [
              set territory no-patches ; This is the territory of a male.
              set lost-territory? true ; male has completely lost his territory, now making him a floater looking for unoccupied females
            ]
          ]
        ]
        set dead-adult-fem dead-adult-fem + 1
        if lists? = true and ticks >= 200 and fem-land-tenure > 0
        [
          set age-adult-fem-died lput age age-adult-fem-died
          set fem-land-tenure-list lput fem-land-tenure fem-land-tenure-list
          set total-cub-list lput total-cubs total-cub-list
          set cub-indep-list lput cub-to-indep cub-indep-list
        ]
        ; Have cubs belonging to dead female die as well:
        if count my-offspring > 0
        [
          ask my-offspring
          [
            ifelse breed = males
            [
              if age-class = "cub"
              [
                set dead-cub-male dead-cub-male + 1
                ;print (word "MALE CUB "who" JUST DIED BECAUSE HIS MOM DIED!")
              ]
              if age-class = "juvenile"
              [
                set dead-juv-male dead-juv-male + 1
                ;print (word "MALE JUVENILE "who" JUST DIED BECAUSE HIS MOM DIED!")
              ]
            ]
            [
              if age-class = "cub"
                [
                  set dead-cub-fem dead-cub-fem + 1
                  ;print (word "FEMALE CUB "who" JUST DIED BECAUSE HER MOM DIED!")
              ]
              if age-class = "juvenile"
                [
                  set dead-juv-fem dead-juv-fem + 1
                  ;print (word "FEMALE JUVENILE "who" JUST DIED BECAUSE HER MOM DIED!")
              ]
            ]
            die
          ]
        ]

        die ; Female dies.
      ]
      if count females = 0 [print (word "NO FEMALES!")
        stop]
  ]

  ; The max age set to 180 months after which male dies:
  ask males with [age >= 180]
  [

    ; Remove male's territory by removing ownership of patches belonging to his females:
    if count females-in-my-territory > 0
    [
      ask females-in-my-territory
      [
        ask territory
          [
            set owner-male nobody
            if view-terr?
            [
              set pcolor ([color] of myself) - 2
            ]
        ]

        ; tell females to remove the male as their 'owner':
        set male-in-my-territory no-turtles
      ]
      if lists? = true and ticks >= 200
      [
        set age-adult-male-died lput age age-adult-male-died
        set male-land-tenure-list lput male-land-tenure male-land-tenure-list
      ]
    ]
    set dead-adult-male dead-adult-male + 1
    die ; Male dies
    if count males = 0 [print (word "NO MALES!")
      stop]
  ]

  ; Mortality process for resident males:
  ask males with [age >= 36 and count females-in-my-territory > 0]
    [
      let m random-float 1
      let res-male-surv-year breed-male-surv ;0.825
      let res-male-surv-month res-male-surv-year ^ (1 / 12)
      if m < (1 - res-male-surv-month)
      [

        ; remove male's territory by removing ownership of patches belonging to his females:
        ask females-in-my-territory
        [
          ask territory
            [
              set owner-male nobody
              if view-terr?
              [
                set pcolor ([color] of myself) - 2
              ]
          ]

          ; tell females to remove the male as 'owner':
          set male-in-my-territory no-turtles
        ]
        if lists? = true and ticks >= 200
        [
          set age-adult-male-died lput age age-adult-male-died
          set male-land-tenure-list lput male-land-tenure male-land-tenure-list
        ]
        set dead-adult-male dead-adult-male + 1
        die ; Male dies
      ]
      if count males = 0 [print (word "NO MALES!")
        stop]
  ]

  ; mortality process for dispersing males:
  ask males with [age >= 36 and count females-in-my-territory = 0]
  [
    let m random-float 1
    let dis-males-surv-year dis-male-surv ;0.7
    let dis-males-surv-month dis-males-surv-year ^ (1 / 12)
    if m < (1 - dis-males-surv-month)
    [
      if lists? = true and ticks >= 200
      [
        set age-adult-male-died lput age age-adult-male-died
      ]
      set dead-adult-male dead-adult-male + 1
      die ; Male disperser dies
    ]
    if count males = 0 [print (word "NO MALES!")
      stop]
  ]

  ; Transient's (different from 'floater' because too young to have had a territory) mortality:
  ask females with [age >= 24 and age < 36]
  [
    let m random-float 1
    let tran-fem-surv-year tran-fem-surv ;0.7
    let tran-fem-surv-month tran-fem-surv-year ^ (1 / 12)
    if m < (1 - tran-fem-surv-month)
    [
      set dead-tran-fem dead-tran-fem + 1
      die ; Female transient dies
    ]
    if count females = 0 [print (word "NO FEMALES!")
      stop]
  ]

  ask males with [age >= 24 and age < 36]
  [
    let m random-float 1
    let tran-males-surv-year tran-male-surv ;0.7
    let tran-males-surv-month tran-males-surv-year ^ (1 / 12)
    if m < (1 - tran-males-surv-month)
    [
      set dead-tran-male dead-tran-male + 1
      die ; Male transient dies
    ]
    if count males = 0 [print (word "NO MALES!")
      stop]
  ]

  ; Juvenile's mortality:
  ask females with [age >= 12 and age < 24]
  [
    let m random-float 1
    let juv-fem-surv-year juv-fem-surv ;0.8
    let juv-fem-surv-month juv-fem-surv-year ^ (1 / 12)
    if m < (1 - juv-fem-surv-month)
    [
      let dead-fem-juv self

      ; remove the dead juvenile from offspring belonging to mom:
      if my-mom != nobody
      [
        ask my-mom
        [
          set my-offspring my-offspring with [self != dead-fem-juv]

          ; if mom now has no offspring then she returns to fertile status:
          if count my-offspring = 0
          [
            set fertile? true
            set color orange ;brown
            set t-parenting 0
          ]
        ]
      ]
      set dead-juv-fem dead-juv-fem + 1
      die ; Juvenile female dies
    ]
    if count females = 0 [print (word "NO FEMALES!")
      stop]
  ]

  ask males with [age >= 12 and age < 24]
  [
    let m random-float 1
    let juv-males-surv-year juv-male-surv ;0.8
    let juv-males-surv-month juv-males-surv-year ^ (1 / 12)
    if m < (1 - juv-males-surv-month)
    [
      let dead-male-juv self

      ; remove the dead juvenile from offspring belonging to mom:
      if my-mom != nobody
      [
        ask my-mom
        [
          set my-offspring my-offspring with [self != dead-male-juv]
          if count my-offspring = 0
          [
            set fertile? true
            set color orange ;brown
            set t-parenting 0
          ]
        ]
      ]
      set dead-juv-male dead-juv-male + 1
      die ; Juvenile male dies
    ]
    if count males = 0 [print (word "NO MALES!")
      stop]
  ]

  ; Cub's mortality:
  ask females with [age < 12]
  [
    let m random-float 1
    let cub-fem-surv-year cub-fem-surv ;0.7
    let cub-fem-surv-month cub-fem-surv-year ^ (1 / 12)
    if m < (1 - cub-fem-surv-month)
    [
      let dead-fem-cub self

      ; remove the dead cub from offspring belonging to mom:
      if my-mom != nobody
      [
        ask my-mom
        [
          set my-offspring my-offspring with [self != dead-fem-cub]

          ; if mom now has no offspring then she returns to fertile status:
          if count my-offspring = 0
          [
            set fertile? true
            set color orange ;brown
            set t-parenting 0
          ]
        ]
      ]
      set dead-cub-fem dead-cub-fem + 1
      die ; Female cub dies
    ]
    if count females = 0 [print (word "NO FEMALES!")
      stop]
  ]

  ask males with [age < 12]
  [
    let m random-float 1
    let cub-males-surv-year cub-male-surv ;0.7
    let cub-males-surv-month cub-males-surv-year ^ (1 / 12)
    if m < (1 - cub-males-surv-month)
    [
      let dead-male-cub self

      ; remove the dead cub from offspring belonging to mom:
      if my-mom != nobody
      [
        ask my-mom
        [
          set my-offspring my-offspring with [self != dead-male-cub]

          ; if mom now has no offspring then she returns to fertile status:
          if count my-offspring = 0
          [
            set fertile? true
            set color orange ;brown
            set t-parenting 0
          ]
        ]
      ]
      set dead-cub-male dead-cub-male + 1
      die ; Male cub dies
    ]
    if count males = 0 [print (word "NO MALES!")
      stop]
  ]
end

;-------------------------------------------------------------------------------------------------------------------------------------------

to update-age-stage-class

  ; Individuals age by one month and might enter a new stage:
  ask females
  [
    set age (age + 1)
    ifelse age < 180
    [
      set age-years (floor (age / 12) + 1)
    ]
    [
      set age-years 15
    ]

    if age = 12
    [
      set age-class "juvenile"
    ]

    ; Female becomes transient :
    if age = 24
    [
      set age-class "transient"
      set size 3
      set color green
      set shape "square"
      let new-transient self

      ; remove the transient from offspring belonging to breeding mom:
      if my-mom != nobody
      [
        ask my-mom
        [
          set my-offspring my-offspring with [self != new-transient]
          set cub-to-indep cub-to-indep + 1
        ]
      ]
    ]

    ; Female becomes fertile:
    if age = 36
    [
      set color orange ;brown
      set my-offspring no-turtles
      set age-class "breeder"
      set fertile? true
      set gestating? false
    ]

    if fertile? = false
    [
      set color orange ;violet
    ]
  ]

  ask males
  [
    set age (age + 1)

    ; set age in years for use in probability of winning challenge matrix:
    ifelse age < 180
    [
      set age-years (floor (age / 12) + 1)
    ]
    [
      set age-years 15
    ]
    if age = 12
    [
      set age-class "juvenile"
    ]

    ; Male becomes transient and leaves natal territory:
    if age = 24
    [
      set age-class "transient"
      set color green
      set shape "triangle"
      set size 5
      let new-transient self

      ; Remove the transient from cubs belonging to breeding mom:
      if my-mom != nobody
      [
        ask my-mom
        [
          set my-offspring my-offspring with [self != new-transient]
          set cub-to-indep cub-to-indep + 1
        ]
      ]
      set females-in-my-territory no-turtles
    ]

    if age = 36
    [
      set color blue ;red
      set territory no-patches
      set nearby-females no-patches
      set age-class "breeder"
      set dominant-males no-turtles
    ]
  ]

  ; Update agent-sets:
  set cub-females females with [age-class = "cub"]
  set cub-males males with [age-class = "cub"]
  set juvenile-females females with [age-class = "juvenile"]
  set transient-females females with [age-class = "transient"]
  set breeding-females females with [age-class = "breeder"]
  set juvenile-males males with [age-class = "juvenile"]
  set transient-males males with [age-class = "transient"]
  set breeding-males males with [age-class = "breeder"]
end

;-------------------------------------------------------------------------------------------------------------------------------------------

to grow-female-territory
  let target nobody
  let vacant no-patches
  let owned no-patches
  let subord no-patches
  let is-subord 0
  let new-patches no-patches
  let calling-fem self
  let age-of-calling-fem age-years
  let edge-patches territory with [count neighbors4 with [owner-fem = calling-fem] = 1] ; selects patches along inner edge of territory
  let patch-with-lowest-prey no-patches

  ; find patch along inner edge of territory with lowest prey value:
  if count edge-patches > 0
  [
    set patch-with-lowest-prey min-one-of edge-patches [prey]
  ]
  ask territory
  [

    ; Add unoccupied neighbor patches as potential targets:
    set vacant (patch-set vacant neighbors4 with [owner-fem = nobody]) ; R1
                                                                                      ; using neighbors4 because it reduces the likelihood that female territories "interweaving" with each other

    ; Find occupied neighbor patches:
    set owned (patch-set owned neighbors4 with [owner-fem != nobody and owner-fem != calling-fem]) ; R1
  ]

  ask owned
  [

    ; Determine if owner of neighbor patches is subordinant to the calling female:
    set is-subord subord? [age-years] of owner-fem age-of-calling-fem  ; R2
  ]

  ; Add subord neighbor patches as potential targets:
  set subord owned with [is-subord = true] ; R2

  ; Set of all potential targets:
  set new-patches (patch-set new-patches vacant subord)

  ; Choose as target the one potential target with highest prey:
  if any? new-patches
  [
    ask new-patches
    [
      ifelse any? vacant
      [
        ; If there is any vacant patch, next check whether there is any subord patch:
        ifelse any? subord
        [
          ifelse [prey] of max-one-of vacant [prey] = [prey] of max-one-of subord [prey] ; Maximum quality is the same
          [
            set target max-one-of vacant [prey]  ; R4
          ]
          [
            ; Maximum quality is not the same:
            ifelse [prey] of max-one-of subord [prey] > [prey] of max-one-of vacant [prey]
            [
              ; Maximum quality of subord patch is larger than maximum quality of vacant patch;
              ; select subord with probability 25%
              let b (random-float 1)
              ifelse b < dom-take-subord ;0.25
              [set target max-one-of subord [prey]] ; R5
              [set target max-one-of vacant [prey]] ; R6
            ]
            [
              ; Maximum quality of subord patch is smaller than maximum quality of vacant patch;
              ; just take maximum quality new-patch:
              set target max-one-of new-patches [prey] ; R4
            ]
          ]
        ]
        [

          ; There is no subord patch:
          set target max-one-of vacant [prey] ; R3
        ]
      ]
      [
        ; There is no vacant patch, but there must be at least one subord patch:
        set target max-one-of subord [prey] ; R7
      ]
    ]
  ]
  if target != nobody
  [
    ; Add target patch to territory of the current animal:
    set territory (patch-set territory target) ; this is the territory of the calling tiger - we are still in its context

    let old-owner-fem [owner-fem] of target
    let owning-male [self] of male-in-my-territory

    ; Tell target patch that is has new owner-fem and owner-male:
    ask target
    [
      set owner-fem calling-fem
      set owner-male owning-male
    ]

    ; Tell the original owner-fem of the target patch to remove the target patch from its territory:
    if old-owner-fem != nobody ;
    [
      ask old-owner-fem
      [
        set territory territory with [ owner-fem != calling-fem ]
        set total-food (prop-prey-crop * sum [prey] of territory)

        ifelse count territory < 2 ; Sometimes a territory of a female is removed because another female takes her territory.
                                   ; In this case the first female has to move and find a new place to start her territory
        [
          ;print (word "TERRITORY LESS THAN 2!!")
          let dying-female self
          ask territory
          [
            set owner-fem nobody
            set owner-male nobody
            set pcolor scale-color black prey min-prey max-prey
          ]
          ask male-in-my-territory
          [
            set females-in-my-territory females-in-my-territory with [self != dying-female]
            if count females-in-my-territory = 0
            [
              set territory no-patches
              set lost-territory? true ; male has completely lost his territory, now making him a floater looking for unoccupied females
            ]
          ]
          ;print (word "FEMALE "who" JUST DIED FROM TOO SMALL TERRITORY!")

          ; If female has offspring then they die as well:
          if count my-offspring > 0
          [
            ask my-offspring
            [
              ifelse breed = males
              [
                if age-class = "cub"
                [
                  set dead-cub-male dead-cub-male + 1
                ]
                if age-class = "juvenile"
                [
                  set dead-juv-male dead-juv-male + 1
                ]
              ]
              [
                if age-class = "cub"
                [
                  set dead-cub-fem dead-cub-fem + 1
                ]
                if age-class = "juvenile"
                [
                  set dead-juv-fem dead-juv-fem + 1
                ]
              ]
              die
            ]
          ]
          if lists? = true and ticks >= 200 and fem-land-tenure > 0
          [
            set age-adult-fem-died lput age age-adult-fem-died
            set fem-land-tenure-list lput fem-land-tenure fem-land-tenure-list
            set total-cub-list lput total-cubs total-cub-list
            set cub-indep-list lput cub-to-indep cub-indep-list
          ]
          set dead-fem-starv dead-fem-starv + 1
          set dead-adult-fem dead-adult-fem + 1
          die ; old owner female dies because territory became too small
        ] ; count territory < 6
        [
          ; count territory >= 6:
          if target = patch-here ; if the old owner female happens to be standing on the patch that was taken by the other female, then the first female has to move to one of her own patches
          [
            move-to one-of patches with [owner-fem = myself]
          ]
          ask male-in-my-territory
          [
            set territory territory with [self != target]
          ]
        ]
      ] ; ask old-owner-fem
    ] ; if old-owner-fem != nobody

    ; Calculate total prey of territory (we are still in ask breeding-females context):
    set total-food (prop-prey-crop * sum [prey] of territory)

    ; When female has met minimum food requirements, she can substitute one of her territory patches for an adjacent patch with higher prey:
    if total-food > min-wn-terr and is-patch? patch-with-lowest-prey ; R9 corresponds to this "if" statement
    [
      if [prey] of target > [prey] of patch-with-lowest-prey
      [
        ask patch-with-lowest-prey
        [
          ifelse any? neighbors4 with [self = target]
          [
            stop
          ]
          [
            set cluster nobody
            set owner-fem nobody
            set owner-male nobody
            set cluster-id ""
            set pcolor scale-color black prey min-prey max-prey
            ask [male-in-my-territory] of calling-fem
            [
              set territory territory with [self != patch-with-lowest-prey]
            ]
          ]
        ]
        set territory territory with [owner-fem = calling-fem]
        set total-food (prop-prey-crop * sum [prey] of territory)
      ]
    ] ; if total-food > min-wn-terr
  ] ; if target != nobody
end

;-------------------------------------------------------------------------------------------------------------------------------------------

to update-female-territory ; females procedure

  ask breeding-females
  [
    set food-initial total-food


    let age-of-calling-fem age-years ;
    let calling-fem self

    ;; Within one time step (1 month), a female has 48 attempts to enlarge her territory until it provides enough food/prey:
    ; Thus, she can potentially add 48 pixels to her territory which equals 3 sq km.
      ; 3 km is approximately the degree of fluctuation in female territory size based on previous studies (e.g., Sunquist 1981)
    let i 0
    while [i < terr-growth and total-food < max-wn-terr]
    [
      ; If territory not yet good enough:
      grow-female-territory
      set i i + 1
    ]

    if total-food > min-wn-terr
    [
      set fem-land-tenure fem-land-tenure + 1
    ]
    ask territory
    [
      if view-terr?
      [
        set pcolor ([color] of myself) - 2
      ]
      if owner-fem != myself [ user-message "Something wrong" ]
    ]
    if count territory > 0
    [
      move-to one-of patches with [owner-fem = myself]
    ]
  ] ; ask breeding-females

  find-clusters ; R10
  remove-clusters ; R10

end ; update-female-territory

;-------------------------------------------------------------------------------------------------------------------------------------------

to calculate-fem-centroid
  ; Identifies the patch that is the centroid of the female territory.
  ; Since the female agent can move around her whole territory, the centroid is used by males to determine the location of female territories relative to himself rather than the female agent.

  ; Clears all previous centroid locations because territories have changed:
  if all-female-centroids != 0
  [
    ask all-female-centroids ; this is a patch-set
    [
      set owner-fem-centroid nobody
    ]
  ]

  ; Calculates centroid as mean X and Y coordinates of territory:
  ask breeding-females
  [
    if count territory > 0
    [
      set XY [list pxcor pycor] of territory ; Creates a list of XY pairs
      set X map [ ?1 -> (item 0 ?1) ] XY              ; Extracts the X coordinates
      set Y map [ ?1 -> (item 1 ?1) ] XY              ; Extracts the Y coordinates

      let meanX mean X ; Mean X for centroid of female territory
      let meanY mean Y ; Mean Y for centroid of female territory
      set meanXY patch meanX meanY ; Centroid patch of female territory
      ask meanXY
      [
        set owner-fem-centroid myself
      ]
    ]
  ]
  set all-female-centroids patch-set [meanXY] of breeding-females ; Find all the centroids of females
end

;-------------------------------------------------------------------------------------------------------------------------------------------

to establish-or-update-male-territory
  ask breeding-males
  [
    let calling-male self
    let my-fem-terr no-patches
    let unowned-fem-terr no-patches
    let target-fem-terr no-patches
    let closest-unowned-fem-terr no-patches
    let losing-male-fem-terr no-patches
    let fem-neighbors no-patches
    set initial-females females-in-my-territory
    ; Update the male's territory, which consists of the territories of his females, because female territories might
    ; have changed:

    if count females-in-my-territory > 0 ; R1 corresponds to this "if" statement
                                         ; the following "if" procedure makes sure to update the male home range according
                                         ; to any changes in the ranges of females he already "owns".
    [

      ; Patches belonging to females of male are selected:
      ;   owner-fem: female owner of a patch
      ;   [females-in-my-territory] of myself: females of calling tiger
      ask females-in-my-territory
      [
        set my-fem-terr (patch-set my-fem-terr territory)
      ]

      ; Erase ownership of previous territory patches because female territories may have
      ; shifted so this has to be updated along with the male territory:
      ask territory [set owner-male nobody]  ; "owner-male" is updated at the end of the procedure

      ; revise the territory of male to match territories of his females

      set territory my-fem-terr
    ]


    ; Identify females who might be added to the males territory:
    ifelse count females-in-my-territory = 0
    [
      ; Dispersing tigers that haven't set up a territory yet.
      ; Select all females with centroids (i.e., meanXY) within
      ; a certain radius around the dispersing male:

      set nearby-females breeding-females with [member? meanXY ([all-female-centroids in-radius male-search-rad] of calling-male)]  ; R2
    ]
    [
      ; Resident males that have a territory and centroid.
      ; Selects all female centroids within a certain radius around existing collection of
      ; female centroids in male's territory:

      let tot-nearby-females-centroids patch-set [all-female-centroids in-radius male-search-rad] of my-fem-centroids  ; R3

      ; Link the female-centroids to the females they belong to:

      let tot-nearby-females breeding-females with [member? meanXY (tot-nearby-females-centroids)]

      ;; Sometimes female territories are adjoining (i.e., neighboring) the male territory but because of the orientation of the respective territories does not register as having
      ; the centroid within 12 patches of male territory centroid. Nevertheless, we consider these females as being nearby and accessible to the male:

      ; identify neighbor patches of territory owned by females:
      ask territory ; Territory of the calling, resident male
      [
        set fem-neighbors (patch-set fem-neighbors neighbors with [member? owner-fem ([self] of breeding-females)])  ; R4
      ]

      ; link those neighbor patches to the females they belong to:
      let adjacent-females breeding-females with [member? self ([owner-fem] of fem-neighbors)]  ; R4

      ; Combine females with territories adjoining male territory with females in the near vicinity:
      let tot-avail-females (turtle-set tot-nearby-females adjacent-females)

      ; Make sure to exclude the existing females in male's territory:
      set nearby-females tot-avail-females with [not member? self ([females-in-my-territory] of calling-male)]
    ]

    if any? nearby-females
    [

      ; identify unowned nearby females to males:
      let unowned-nearby-females nearby-females with [member? self (breeding-females with [count male-in-my-territory = 0])]

      if any? unowned-nearby-females
      [
        ; If less than 6 females in the males territory, one or more females can be added.
        ; We restrict maximum to 6, because otherwise male can expand to include an unrealistic number of female territories:
        ifelse count females-in-my-territory < max-fem
        [

          ; Selects all patches of territories from nearby females with no male:
          let num-poss-fem-add max-fem - count females-in-my-territory
          ifelse count unowned-nearby-females >= num-poss-fem-add
          [
            ; The number of available females is larger or equal to the number of females that still can be added.
            ; if male has a centroid (i.e., a resident male) then selects unowned females closest to the male's centroid:
            let num-add-unowned-females no-turtles
            ifelse [meanXY] of calling-male != 0
            [
              ; Select the right number of females and take the closer ones first:
              set num-add-unowned-females min-n-of num-poss-fem-add unowned-nearby-females [distance [meanXY] of calling-male] ; R5
            ]
            [
              ; if male does not have a centroid (i.e., a dispersing male) then selects unowned females closest to male agent:
              ; That is, it is same as above except but determines female location based on location of self rather than centroid of territory:
              set num-add-unowned-females min-n-of num-poss-fem-add unowned-nearby-females [distance calling-male]
            ]
            ask num-add-unowned-females ; "num-add-unowned-females" is not a number, but a set of females
            [
              set unowned-fem-terr (patch-set unowned-fem-terr territory)
            ]

            ; update the number of females that the male owns:
            set females-in-my-territory (turtle-set females-in-my-territory num-add-unowned-females)

            ; update the owning male:
            ask females-in-my-territory
            [
              set male-in-my-territory turtle-set calling-male
            ]
          ] ; count unowned-nearby-females >= num-poss-fem-add
          [
            ; count unowned-nearby-females < num-poss-fem-add: add them all:
            ask unowned-nearby-females
            [
              set unowned-fem-terr (patch-set unowned-fem-terr territory)
            ]

            ; update the number of females that the male owns:
            set females-in-my-territory (turtle-set females-in-my-territory unowned-nearby-females)

            ; update the owning male:
            ask females-in-my-territory
            [
              set male-in-my-territory turtle-set calling-male
            ]
          ]

          ; update male's territory to include patches belonging to unowned nearby females:
          set territory (patch-set territory unowned-fem-terr)
        ] ; females-in-my-territory < 6
        [
          ; females-in-my-territory >= 6

          ; R6 corresponds to the following "else" portion of the "ifelse" statement
          ; the following allows males with 6 females to select an unowned female that happens to be closer to his centroid
          ; than his already owned females. This ensures that an unowned female doesn't occur within his home range that
          ; can then be occupied by another male:

          let unowned-female-centroids patch-set [meanXY] of unowned-nearby-females

          ; selects the closest unowned female:
          let closest-unowned-female-centroid min-one-of unowned-female-centroids [distance [meanXY] of calling-male]

          ; updates the locations of female centroids belonging to male
          ; because he needs this to calculate how far his farthest female is:
          set my-fem-centroids (patch-set [meanXY] of females-in-my-territory)

          ; selects farthest female that he occupies:
          let farthest-owned-female-centroid max-one-of my-fem-centroids [distance [meanXY] of calling-male]

          ; checks to see if closest unowned female is closer than his farthest owned female:
          if [distance closest-unowned-female-centroid] of meanXY < [distance farthest-owned-female-centroid] of meanXY
          [
            let closest-unowned-female breeding-females with [owner-fem = [owner-fem-centroid] of closest-unowned-female-centroid]
            let farthest-owned-female breeding-females with [owner-fem = [owner-fem-centroid] of farthest-owned-female-centroid]

            ; erase male ownership from farthest female previously owned by male:
            ask farthest-owned-female
            [
              ask territory
              [
                set owner-male nobody
                if view-terr?
                [
                  set pcolor ([color] of myself) - 2
                ]
              ]
              set male-in-my-territory no-turtles
              set too-far-female self
            ]

            ; update territory of male so that it excludes farthest female:
            set territory territory with [ owner-fem != too-far-female ]
            set females-in-my-territory females-in-my-territory with [self != too-far-female]

            ; add the territory of closest female to male:
            ask closest-unowned-female
            [
              set closest-unowned-fem-terr (patch-set closest-unowned-fem-terr territory)
              set male-in-my-territory turtle-set calling-male
            ]
            set territory (patch-set territory closest-unowned-fem-terr)
            set females-in-my-territory (turtle-set females-in-my-territory closest-unowned-female)
          ]
        ]
      ] ; females-in-my-territory >= 6
        ; Handling of nearby unowned females finished.

      ; Next: Males with no territory can also "steal" females owned by resident males:

      ; identify nearby females owned by males (not including self):
      let owned-nearby-females nearby-females with [member? self (breeding-females with [count male-in-my-territory > 0])] ; R7


      ; This process does not apply to one of the initial males or ones that have already lost their territory.
      ; This avoids the initial set of males (whom all have no females initially) from competing with each other
      ; and allows them to distribute through the landscape and reach quasi-stable state.
      ; Further, empirical research suggests that males who previously lost territories through a challenge are very unlikely to establish new territory.
      ; The following also applies only to young males who have not yet any females:
      if any? owned-nearby-females and count females-in-my-territory = 0 and lost-territory? != TRUE and initial-male? != TRUE  ; R7
      [

        ; identify those males owning nearby females:
        let owner-males-of-nearby-fem turtle-set [male-in-my-territory] of breeding-females with [member? self (owned-nearby-females)]

        ; identify all males that have been challenged yet; "dominant-males" is empty first but adds males after
        ; a challenge was unsuccessful. This means that young males do not challenge the same male over and over again:
        let near-unchallenged-males (owner-males-of-nearby-fem with [not member? self ([dominant-males] of myself)])

        ; randomly select one of the nearby unchallenged males to challenge:
        let male-to-challenge one-of near-unchallenged-males  ; R8
        if male-to-challenge != nobody
        [

          set num-challenges num-challenges + 1

          ; select the probability of the calling dispersing male winning challenge against resident male:
          let p prob-winning [age-years] of male-to-challenge [age-years] of calling-male ;; Uses lookup table with probabilities

          ; if dispersing male wins challenge against resident male; this implies that the losing male loses ALL his
          ; territory:
          ifelse random-float 1.0 < p
          [
            ; R9 corresponds to the "if" portion of the "ifelse" statement
            let females-of-losing-male [females-in-my-territory] of male-to-challenge

            ; the territory and females belonging to losing male are updated:
            ask male-to-challenge
            [
              ; females remove losing male from their territories:
              ask females-in-my-territory
              [
                ask territory
                [
                  set owner-male nobody
                  if view-terr?
                  [
                    set pcolor ([color] of myself) - 2
                  ]
                ]

                ; remove the losing male from the agent-set of males owning female:
                ; currently only one male can own a female:
                set male-in-my-territory no-turtles
              ]

              ; erases territory of losing male:
              set territory no-patches
              if lists? = true and ticks >= 200
              [
                set male-land-tenure-list lput male-land-tenure male-land-tenure-list
              ]
              set male-land-tenure 0

              ; removes females from losing resident male:
              set females-in-my-territory no-turtles
              set my-fem-centroids no-patches
              let prob-res-male-dies breed-male-die-chall ;0.6

              ; Losing male either dies or disperses:
              ifelse random-float 1.0 < prob-res-male-dies
              [
                if lists? = true and ticks >= 200
                [
                  set age-adult-male-died lput age age-adult-male-died
                ]
                set dead-male-chall dead-male-chall + 1
                set dead-adult-male dead-adult-male + 1
                die
              ]
              [
                set lost-territory? TRUE
                set color blue ;cyan
                male-select-location
              ]
            ]

            ; R10
            ; infanticide.
            ; all cubs belonging to females of losing male are killed by new resident male:
            ask females-of-losing-male
            [
              set losing-male-fem-terr (patch-set losing-male-fem-terr territory)
              if count my-offspring > 0
              [
                ask my-offspring
                [
                  if age-class = "cub"
                  [
                    let prob-cub-killed cub-die-infant ;0.79
                    if random-float 1.0 < prob-cub-killed
                    [
                      set num-infanticide num-infanticide + 1
                      die
                    ]
                  ]
                  if age-class = "juvenile"
                  [
                    let prob-juv-killed juv-die-infant ;0.24
                    if random-float 1.0 < prob-juv-killed
                    [
                      set num-infanticide num-infanticide + 1
                      die
                    ]
                  ]
                ]
              ]

              ; losing the cubs triggers estrous in the female and returns them to "fertile" status:
              set fertile? true ; Applies to all females-of-losing-male
              set color orange ;brown
              set t-parenting 0
            ]

            ; winning male updates his territory and number of females he took from the losing male:
            set females-in-my-territory females-of-losing-male

            ; update the owning male:
            ask females-in-my-territory
            [
              set male-in-my-territory turtle-set calling-male
            ]
            set territory (patch-set territory losing-male-fem-terr)
          ]

          ;if dispersing male loses challenge against resident male:
          [
            set dominant-males (turtle-set dominant-males male-to-challenge) ; R11
                                                                             ; print (word "male "who" lost to " [self] of dominant-males)
            let prob-dis-male-dies tran-male-die-chall ; 0.25
            if random-float 1.0 < prob-dis-male-dies
            [
              if lists? = true and ticks >= 200
              [
                set age-adult-male-died lput age age-adult-male-died
              ]
              set dead-adult-male dead-adult-male + 1
              die
            ]
          ]
        ] ; if male-to-challenge != nobody
      ]
    ] ; if any? nearby-females

    ; move male to a patch within his territory and update land tenure time:
    if count females-in-my-territory > 0
    [
      set male-land-tenure male-land-tenure + 1
      move-to one-of territory
    ]

    ; update these patches so they are now owned by male:
    ask territory
    [
      set owner-male calling-male
      if view-terr?
      [
        set pcolor ([color] of myself) - 2
      ]
    ]
    set my-fem-centroids (patch-set [meanXY] of females-in-my-territory)
  ] ; ask males
end ; establish-or-update-male-territory

;-------------------------------------------------------------------------------------------------------------------------------------------

to calculate-male-centroid

  ; Calculate territory centroid for each male.
  ; This is used by males to determine nearest females to centroid of his territory:
  ask breeding-males
  [
    if count females-in-my-territory > 0
    [
      set XY [list pxcor pycor] of territory
      set X map [ ?1 -> (item 0 ?1) ] XY
      set Y map [ ?1 -> (item 1 ?1) ] XY
      let meanX mean X ; mean X for centroid of male territory
      let meanY mean Y ; mean Y for centroid of male territory
      set meanXY patch meanX meanY ; centroid patch of male territory
                                   ; measure distance from natal range to new territory only for males setting up territory for first time
      if count initial-females = 0 and lost-territory? != true and initial-male? != true
      [
        set male-disp-dist [distance [meanXY] of myself] of natal-origin
        if lists? = true and ticks >= 200
        [
          set male-disp-dist-list lput male-disp-dist male-disp-dist-list
        ]
      ]
    ]
  ]
end

;-------------------------------------------------------------------------------------------------------------------------------------------

to find-clusters ; observer procedure

  ; finds contiguous clusters of patches in each female territory:
  ask patches [set cluster nobody]
  loop
  [
    let seed one-of patches with [(owner-fem != nobody) and (cluster = nobody)]

    if seed = nobody
    [
      show-clusters
      stop
    ]

    ask seed
    [
      set cluster self
      grow-cluster
    ]
  ]
end

;-------------------------------------------------------------------------------------------------------------------------------------------

to grow-cluster ; patch procedure

  ask neighbors4 with [(cluster = nobody) and (owner-fem = [owner-fem] of myself)]
  [
    set cluster [cluster] of myself
    grow-cluster ; recursion
  ]
end

;-------------------------------------------------------------------------------------------------------------------------------------------

to show-clusters ; patch procedure
  let counter 0
  loop
  [
    ;; pick a random patch we haven't labeled yet:
    let p one-of patches with [(cluster-id = "") and (cluster != nobody)]
    if p = nobody [ stop ]

    ;; give all patches in the chosen patch's cluster
    ;; the same label:
    ask p
    [
      ask patches with [(cluster = [cluster] of myself) and (owner-fem != nobody)]
      [
        set cluster-id counter
      ]
    ]
    set counter counter + 1
  ]
end

;-------------------------------------------------------------------------------------------------------------------------------------------

to remove-clusters
  ask breeding-females
  [
    ; determine how many different clusters my territory has:
    let clusters-in-my-territory no-patches
    ask territory
    [
      if not member? cluster clusters-in-my-territory
      [
        set clusters-in-my-territory (patch-set clusters-in-my-territory cluster)
      ]
    ]

    if count clusters-in-my-territory = 1
    [
      stop
    ] ; nothing to remove

    ; determine largest cluster:
    let biggest-cluster nobody
    let nmax 0
    ask clusters-in-my-territory
    [
      let n count patches with [ cluster-id = [cluster-id] of myself ] ; myself because "with" sets a new context
                                                                       ;print (word "Cluster " self ": " n)

      if n > nmax
      [
        set biggest-cluster self
        set nmax n
      ]
    ]

      ; remove all but the biggest cluster:
    ask territory
    [
      if cluster != biggest-cluster
      [
        set cluster nobody
        set owner-fem nobody
        set owner-male nobody
        set cluster-id ""
        set pcolor scale-color black prey min-prey max-prey
      ]
    ]

    ; Update male and female territories:
    ask male-in-my-territory
    [
      let calling-male self
      set territory territory with [owner-male = calling-male]
    ]

    set territory territory with [owner-fem = myself]
    set total-food (prop-prey-crop * sum [prey] of territory) ; recalculate total food in female territory

    if count territory < 2 ; female dies if territory becomes smaller than 2 patches
      [
        let dying-female self
        ask territory
        [
          set owner-fem nobody
          set owner-male nobody
          set pcolor scale-color black prey min-prey max-prey
        ]
        ask male-in-my-territory
        [
          set females-in-my-territory females-in-my-territory with [self != dying-female]
          if count females-in-my-territory = 0
          [
            set territory no-patches
            set lost-territory? true ; male has completely lost his territory, now making him a floater looking for unoccupied females
          ]
        ]

        ; If female has offspring then they die as well:
        if count my-offspring > 0
        [
          ask my-offspring
          [
            ifelse breed = males
            [
              if age-class = "cub"
              [
                set dead-cub-male dead-cub-male + 1
                ;print (word "MALE CUB "who" JUST DIED BECAUSE HIS MOM DIED!")
              ]
              if age-class = "juvenile"
              [
                set dead-juv-male dead-juv-male + 1
                ;print (word "MALE JUVENILE "who" JUST DIED BECAUSE HIS MOM DIED!")
              ]
            ]
            [
              if age-class = "cub"
              [
                set dead-cub-fem dead-cub-fem + 1
                ;print (word "FEMALE CUB "who" JUST DIED BECAUSE HER MOM DIED!")
              ]
              if age-class = "juvenile"
              [
                set dead-juv-fem dead-juv-fem + 1
                ;print (word "FEMALE JUVENILE "who" JUST DIED BECAUSE HER MOM DIED!")
              ]
            ]
            die
          ]
        ]
        if lists? = true and ticks >= 200 and fem-land-tenure > 0
        [
          set age-adult-fem-died lput age age-adult-fem-died
          set fem-land-tenure-list lput fem-land-tenure fem-land-tenure-list
          set total-cub-list lput total-cubs total-cub-list
          set cub-indep-list lput cub-to-indep cub-indep-list
        ]
        set dead-fem-starv dead-fem-starv + 1
        set dead-adult-fem dead-adult-fem + 1
        die ; old owner female dies because territory became too small
    ]
    if count females = 0 [print (word "NO FEMALES!")
      stop]
    move-to one-of patches with [owner-fem = myself] ; female moves to one of her territory patches in case she was on patch from cluster just removed

    if count my-offspring > 0
    [
      ask my-offspring
      [
        ; make sure her offpsring also remain on her territory in case they were previously on cluster that was removed
        move-to one-of patches with [owner-fem = [my-mom] of myself]
      ]
    ]
  ] ; ask females
end

;-------------------------------------------------------------------------------------------------------------------------------------------

to female-starvation
  ask breeding-females
  [
    set tot-food-change total-food - food-initial

    ; Female dies if she does not have enough food and her territory size is not increasing in size:
    if total-food < min-wn-terr and tot-food-change <= 0
    [
      let dying-female self
      ask territory
      [
        set cluster nobody
        set owner-fem nobody
        set owner-male nobody
        set cluster-id ""
        set pcolor scale-color black prey min-prey max-prey
      ]
      if count male-in-my-territory > 0
      [
        ask male-in-my-territory
        [
          set females-in-my-territory females-in-my-territory with [self != dying-female]
          if count females-in-my-territory = 0
          [
            set territory no-patches
            set lost-territory? true ; male has completely lost his territory, now making him a floater looking for unoccupied females
          ]
        ]
      ]
      if count my-offspring > 0
      [
        ask my-offspring
        [
          ifelse breed = males
          [
            if age-class = "cub"
            [
              set dead-cub-male dead-cub-male + 1
              ;print (word "MALE CUB "who" JUST DIED BECAUSE HIS MOM DIED!")
            ]
            if age-class = "juvenile"
            [
              set dead-juv-male dead-juv-male + 1
              ;print (word "MALE JUVENILE "who" JUST DIED BECAUSE HIS MOM DIED!")
            ]
          ]
          [
            if age-class = "cub"
            [
              set dead-cub-fem dead-cub-fem + 1
              ;print (word "FEMALE CUB "who" JUST DIED BECAUSE HER MOM DIED!")
            ]
            if age-class = "juvenile"
            [
              set dead-juv-fem dead-juv-fem + 1
              ;print (word "FEMALE JUVENILE "who" JUST DIED BECAUSE HER MOM DIED!")
            ]
          ]
          die ; Female offspring dies because she dies
        ]
      ]
      if lists? = true and ticks >= 200 and fem-land-tenure > 0
      [
        set age-adult-fem-died lput age age-adult-fem-died
        set fem-land-tenure-list lput fem-land-tenure fem-land-tenure-list
        set total-cub-list lput total-cubs total-cub-list
        set cub-indep-list lput cub-to-indep cub-indep-list
      ]
      set dead-adult-fem dead-adult-fem + 1
      set dead-fem-starv dead-fem-starv + 1

      die ; Female dies
    ]
  ]
  if count females = 0 [print (word "NO FEMALES!")
    stop]
end

;-------------------------------------------------------------------------------------------------------------------------------------------

to parenting

  ; ensures female does not give-birth again while weaning her litter:
  ask breeding-females with [fertile? = false and gestating? = false]
  [
    set t-parenting (t-parenting + 1)

    ; female returns to fertile status after raising offspring to adulthood:
    if t-parenting >= 24
    [
      set fertile? true
      set color orange ;brown
      set t-parenting 0
    ]
  ]
end

;-------------------------------------------------------------------------------------------------------------------------------------------

to gestation

  ; this is the gestation countdown
  ; when gestation period of ~9 months is over females produce litter:
  ask breeding-females with [gestating? = true]
  [
    ifelse t-gestation > 0
    [
      set t-gestation t-gestation - 1
    ]
    [
      give-birth
      set gestating? false
    ]
  ]
end

;-------------------------------------------------------------------------------------------------------------------------------------------

to give-birth
  set my-offspring no-turtles

  ; litter size ranges from 2 - 5
  ; the probabilities of having 2,3,4,5 cubs are 0.23,0.81,0.17,0.02
  ; the following lines draws a random number and chooses the
  ; number of cubs based on their likelihoods:
  let prob-table [ 0.23 0.81 0.98 1 ]
  ;let prob-table [ 0.1 0.9 1 ]
  ;let prob-table [ 0.2 0.4 0.6 0.8 1 ]

  let prob random-float 1.0

  ; selects one of the probabilities (e.g., 0.23) based on the randomly drawn number:
  let numb-of-cubs-prob reduce [ [?1 ?2] -> ifelse-value ( prob <= ?1) [?1] [?2] ] prob-table

  ; a list of lists that links the probabilites to number of cubs:
  let num-poss-cubs [[0.23 2][0.81 3][0.98 4][1 5]]
  ;let num-poss-cubs [[0.1 2][0.9 3][1 4]]
  ;let num-poss-cubs [[0.2 1][0.4 2][0.6 3][0.8 4][1 5]]

  ; uses reporter (see bottom of procedure) to select number of cubs based on likelihood value:
  let number-of-cubs lookup numb-of-cubs-prob num-poss-cubs
  if lists? = true and ticks >= 200
  [
    set litter-size-list lput number-of-cubs litter-size-list
  ]
  repeat number-of-cubs
  [
    let coin random 2

    ; if number 0 then male, and if number is 1 then female:
    if-else coin = 0
    [
      ask n-of 1 patches with [owner-fem = myself]
      [
        sprout-males 1
        [
          let new-male-cub self
          set my-mom owner-fem

          ; update the number of current cubs belonging to mom:
          ask my-mom
          [
            set my-offspring (turtle-set my-offspring new-male-cub)
          ]
          set age 0
          set age-years (floor (age / 12) + 1)
          ifelse hide-nonbreeders?
          [
            set hidden? true
          ]
          [
            set shape "circle"
            set size 4
            set color cyan ;yellow
          ]
          set name (word "t" who)
          set age-class "cub"
          set cub-males (turtle-set cub-males new-male-cub)
          set natal-origin [meanXY] of my-mom
        ]
      ]
    ]
    [
      ask n-of 1 patches with [owner-fem = myself]
      [
        sprout-females 1
        [
          let new-fem-cub self
          set my-mom owner-fem

          ; update the number of current cubs belonging to mom:
          ask my-mom
          [
            set my-offspring (turtle-set my-offspring new-fem-cub)
          ]
          set age 0
          set age-years (floor (age / 12) + 1)
          ifelse hide-nonbreeders?
          [
            set hidden? true
          ]
          [
            set shape "circle"
            set size 4
            set color cyan ;yellow
          ]
          set name (word "t" who)
          set age-class "cub"
          set male-in-my-territory no-turtles
          set cub-females (turtle-set cub-females new-fem-cub)
          set natal-origin [meanXY] of my-mom
        ]
      ]
    ]
  ]
  set total-cubs total-cubs + number-of-cubs
  set num-litters num-litters + 1
  ;set fertile? false ;;V You sure?
  set color orange ;violet
end

; selects the number of cubs associated with the likelihood:
to-report lookup [key entries]
  report item 1 first filter [ ?1 -> first ?1 = key ] entries
end

;-------------------------------------------------------------------------------------------------------------------------------------------

to prob-mating

  ; females > 3 and < 4 years old have an annual probability of 90% of successfully reproducing when fertile:
  ask breeding-females with [age < 48 and fertile? = true and count male-in-my-territory > 0 and fem-land-tenure > 0]
  [
    let r random-float 1
    let prob-mate-year fem-three-mate ;0.9
    let prob-mate-month prob-mate-year ^ (1 / 12)
    if r < prob-mate-month
    [
      set gestating? true
      set t-gestation 3 + random 2
      set fertile? false
    ]
  ]

  ; females >= 4 years old have an annual probability of 100% of successfully reproducing when fertile:
  ask breeding-females with [age >= 48 and fertile? = true and count male-in-my-territory > 0 and fem-land-tenure > 0]
  [
    let r random-float 1
    let prob-mate-year fem-four-mate ;1
    let prob-mate-month prob-mate-year ^ (1 / 12)
    if r < prob-mate-month
    [
      set gestating? true
      set t-gestation 3 + random 2
      set fertile? false
    ]
  ]
end

;-------------------------------------------------------------------------------------------------------------------------------------------

to male-select-location
  ; males older than 3 years finally settle down in an area to establish a territory with access to females:
  ; select all females (except their sisters) with no male occupying their territory:
  ; R1:
  let unoccupied-females breeding-females with [my-mom != [my-mom] of myself and count male-in-my-territory = 0]

  ; select all females with a male occupying their territory:
  let occupied-females breeding-females with [count male-in-my-territory > 0]

  ; select territory centroids of all females with no male (i.e., available females):
  let avail-fem-centroids all-female-centroids with [member? owner-fem-centroid ([self] of unoccupied-females)]

  let near-avail-fem-centroids [avail-fem-centroids in-radius male-disp-rad] of natal-origin

  ; ideal site is one where no other adult male occurs (including another male with no territory) and is within dispersal distance from calling male:
  let ideal-site one-of near-avail-fem-centroids with [not any? males-here with ; this would be a transient who just moved there
    [age >= 36 and count females-in-my-territory = 0]]

  ; select territory centroids of all females with a male (i.e., potential females):
  let pot-fem-centroids all-female-centroids with [member? owner-fem-centroid ([self] of occupied-females)]; /R1

  let near-pot-fem-centroids [pot-fem-centroids in-radius male-disp-rad] of natal-origin

  ; male adds the male who occupies his mom as a dominant male so that he doesn't add his mom to his territory:
  if my-mom != nobody and initial-male? != true
  [
    set dominant-males (turtle-set dominant-males [male-in-my-territory] of my-mom)
  ]

  ; selects females from males that calling male has not challenged:
  if count dominant-males != 0
  [
    ; first selects females belonging to dominant males (i.e., males that have beaten calling male in challenge):
    ; R2:

    let fem-of-dom-males turtle-set [females-in-my-territory] of dominant-males

    ; then selects those territory centroids of females belonging to a male but not one of dominant males:
    set near-pot-fem-centroids near-pot-fem-centroids with [not member? owner-fem-centroid ([self] of fem-of-dom-males)]
    ; /R2
  ]

  ; if no ideal site then calling male moves to non-ideal site with occupied female:
  let non-ideal-site one-of near-pot-fem-centroids with [not any? other males-here with ; this would be a transient who just moved there
    [age >= 36 and count females-in-my-territory = 0]] ;R4

  ; male prefers to add "available" females (i.e., unoccupied by other male) to his territory):
  ifelse ideal-site = nobody
  [
    ifelse non-ideal-site = nobody
    [
      if lists? = true and ticks >= 200
      [
        set age-adult-male-died lput age age-adult-male-died
      ]
      set dead-adult-male dead-adult-male + 1
      die
    ]
    [
      move-to non-ideal-site
    ]
  ]
  [
    ; moves to ideal site if exists:
    move-to ideal-site ;R3
  ]
end

;-------------------------------------------------------------------------------------------------------------------------------------------

to female-select-location

  ; females older than 3 years finally settle down in an area where there is no other female in a given radius:
  ; first she selects those patches within radius and inside park:

  let in-radius-sites [patches in-radius fem-disp-rad] of natal-origin ; R1 ; 132
                                                                       ;132 equals 33 kilometers, which is maximum female dispersal distance observed in Smith 1993
  let potential-sites in-radius-sites ;with [is-park?]


  ; selects potentially good sites with no other female territory in radius of 2 kilometers:
  let pot-good-sites potential-sites with [not any? other females-here and not any? patches in-radius fem-ideal-location with [owner-fem != nobody]]  ; R2

  ; selects ideal site which is the patch with the highest mean prey value in the 2-km radius area around potential site:
  let ideal-site max-one-of pot-good-sites [mean [prey] of patches in-radius fem-ideal-location] ; R3

  ; female will takeover mom's territory if no potential site exists:
  ifelse ideal-site = nobody
  [
    ; It is possible for other female in litter to have already taken over mom's territory, so now mom of calling female is already dead:
    ; selects potential sites that do not have any other female territory within 1 km radius:
    let pot-sites potential-sites with [not any? other females-here and not any? patches in-radius fem-marg-location with [owner-fem != nobody]] ; R4
    ifelse count pot-sites > 0
    [

      ; calling female moves to site with no female territory in 1-km radius area:
      move-to max-one-of pot-sites [mean [prey] of patches in-radius fem-marg-location]  ; R5
      set terr-orig patch-here

      ; we measure the dispersal distance from her natal origin to the centroid of her new territory:
      set fem-disp-dist [distance [terr-orig] of myself] of natal-origin
      if lists? = true and ticks >= 200
      [
        set fem-disp-dist-list lput fem-disp-dist fem-disp-dist-list
      ]
      set territory patch-set terr-orig
      set owner-fem self
    ]
    ; R6
    [
      ; if no sites with no female territory within 1-km radius area exists, then calling-female dies because there is nowhere suitable to go:
      set dead-adult-fem dead-adult-fem + 1
      die
    ]
  ] ; ifelse ideal-site = nobody
  [
    move-to ideal-site ; R3
    set terr-orig patch-here

    ; we measure the dispersal distance from her natal origin to the centroid of her new territory:
    set fem-disp-dist [distance [terr-orig] of myself] of natal-origin
    if lists? = true and ticks >= 200
    [
      set fem-disp-dist-list lput fem-disp-dist fem-disp-dist-list
    ]
    set territory patch-set terr-orig
    set owner-fem self
  ]
end

;-------------------------------------------------------------------------------------------------------------------------------------------

to update-plots-and-outputs
  plot-age-distribution
  plot-female-hr
  plot-male-hr
  ifelse any? breeding-males
  [set mean-male-territory-size mean [count territory / 16] of breeding-males with [count females-in-my-territory > 0]]
  [set mean-male-territory-size 0]
  ifelse any? breeding-females
  [set mean-female-territory-size mean [count territory / 16] of breeding-females]
  [set mean-female-territory-size 0]
  ifelse not any? males or not any? females
  [
    set dead-male-chall "NA"
    set dead-fem-starv "NA"
    set dead-adult-fem "NA"
    set dead-adult-male "NA"
    set dead-cub-male "NA"
    set dead-cub-fem "NA"
    set dead-juv-male "NA"
    set dead-juv-fem "NA"
    set dead-tran-male "NA"
    set dead-tran-fem "NA"
    set num-infanticide "NA"
    set num-breeding-males "NA"
    set num-breeding-females "NA"
    set num-cub-males "NA"
    set num-cub-females "NA"
    set num-juv-males "NA"
    set num-juv-females "NA"
    set num-transient-males "NA"
    set num-transient-females "NA"
    set total-males "NA"
    set total-females "NA"
    set num-fem-w-off "NA"
    set mean-female-territory-size "NA"
    set mean-male-territory-size "NA"
  ]
  [
    set num-breeding-males count breeding-males
    set num-breeding-females count breeding-females
    set num-cub-males count cub-males
    set num-cub-females count cub-females
    set num-juv-males count juvenile-males
    set num-juv-females count juvenile-females
    set num-transient-males count transient-males
    set num-transient-females count transient-females
    set total-males count males
    set total-females count females
    set num-fem-w-off count breeding-females with [count my-offspring > 0]
  ]
end

;-------------------------------------------------------------------------------------------------------------------------------------------

to plot-male-hr
  ;let male-with-hr breeding-males with [count females-in-my-territory > 0]
  set-plot-x-range 0 200
  set-plot-y-range 0 5
  set-histogram-num-bars 20
  histogram [count territory / 16] of breeding-males with [count females-in-my-territory > 0]
end

;-------------------------------------------------------------------------------------------------------------------------------------------

to plot-female-hr
  ;let female-with-hr breeding-females with [count territory > 5]
  set-plot-x-range 0 50
  set-plot-y-range 0 10
  set-histogram-num-bars 10
  histogram [count territory / 16] of breeding-females
end

;-------------------------------------------------------------------------------------------------------------------------------------------

to plot-age-distribution
  let all-tigers (turtle-set females males)
  set-current-plot "Age distribution"
  set-plot-x-range 1 16
  set-plot-y-range 0 10
  set-histogram-num-bars 15
  histogram [age-years] of all-tigers
end

;-------------------------------------------------------------------------------------------------------------------------------------------

to make-movie

  ;; prompt user for movie location
  user-message "First, save your new movie file (choose a name ending with .mov)"
  let path user-new-file
  if not is-string? path [ stop ]  ;; stop if user canceled

  ;; run the model
  setup
  set _recording-save-file-name path
  vid:start-recorder
  vid:record-view
  while [ ticks < 10]
    [ go
      vid:record-view ]

  ;; export the movie
  vid:save-recording _recording-save-file-name
  user-message (word "Exported movie to " path)
end

;-------------------------------------------------------------------------------------------------------------------------------------------

to-report prob-winning [ age-res age-dis ]

  ; probabilities that dispersal tigers will successfully win a challenge against a resident male:
  let probabilities [
    [ 3   0.5  0.55 0.65]
    [ 4   0.45 0.5  0.55]
    [ 5   0.4  0.45 0.5]
    [ 6   0.35 0.4  0.45]
    [ 7   0.4  0.45 0.5]
    [ 8   0.45 0.5  0.55]
    [ 9  0.5  0.55 0.6]
    [ 10  0.7  0.75 0.8]
    [ 11  1    1    1]
    [ 12  1    1    1]
    [ 13  1    1    1]
    [ 14  1    1    1]
    [ 15  1    1    1]
  ]
  report item (age-dis - 2) first filter [ ?1 -> first ?1 = age-res ] probabilities
end

;-------------------------------------------------------------------------------------------------------------------------------------------

to-report subord? [ age-nearby-fem age-me ]

  ; is calling female with certain age in row subordinant to female with age corresponding to column (age starting at 3).
  ; Thus, females with age 6-10 are not subordinant to any other ages:
  let sub-true? [
    [ 3   false  false false true true true true true false false false false false]
    [ 4   false  false false true true true true true false false false false false]
    [ 5   false  false false true true true true true false false false false false]
    [ 6   false  false false false false false false false false false false false false]
    [ 7   false  false false false false false false false false false false false false]
    [ 8   false  false false false false false false false false false false false false]
    [ 9   false  false false false false false false false false false false false false]
    [ 10  false  false false false false false false false false false false false false]
    [ 11  false  false false true true true true true false false false false false]
    [ 12  false  false false true true true true true false false false false false]
    [ 13  false  false false true true true true true false false false false false]
    [ 14  false  false false true true true true true false false false false false]
    [ 15  false  false false true true true true true false false false false false]
  ]
  report item (age-me - 2) first filter [ ?1 -> first ?1 = age-nearby-fem ] sub-true?
end

;-------------------------------------------------------------------------------------------------------------------------------------------

to diagnostics
  ask breeding-males with [count females-in-my-territory > 0]
  [
    if count females-in-my-territory > max-fem
    [
      print (word " females belonging to male "who" : " [self] of females-in-my-territory)
      ask females-in-my-territory
      [
        print (word " males belonging to male "who" : " [self] of male-in-my-territory)
      ]
      user-message "too many females"
    ]
    let females-of-other-males turtle-set [females-in-my-territory] of other breeding-males with [count females-in-my-territory > 0]
    ask females-in-my-territory
    [
      if member? self females-of-other-males
      [
        user-message "males are sharing female"
      ]
    ]
  ]
end

;-------------------------------------------------------------------------------------------------------------------------------------------

to write-csv [ #filename #items ]
  ;; #items is a list of the data (or headers!) to write.
  if is-list? #items and not empty? #items
  [ file-open #filename
    ;; quote non-numeric items
    set #items map quote #items
    ;; print the items
    ;; if only one item, print it.
    ifelse length #items = 1 [ file-print first #items ]
    [file-print reduce [ [?1 ?2] -> (word ?1 "," ?2) ] #items]
    ;; close-up
    file-close
  ]
end

to-report quote [ #thing ]
  ifelse is-number? #thing
  [ report #thing ]
  [ report (word "\"" #thing "\"") ]
end

;-------------------------------------------------------------------------------------------------------------------------------------------

to calc-homerange
  ; r:interactiveShell to see underlying R for NetLogo and ctrl+Enter to submit command in the shell
  ; create an empty data.frame"
  r:eval "turtles <- data.frame()"

  ; merge the name, X- and Y-lists of all females to one big data.frame

  ask breeding-females with [count territory > 5]
  [
    (r:putdataframe "turtle" "X" X "Y" Y)
    r:eval (word "turtle <- data.frame(turtle, name = '" name "')")
    let turtleX r:get "turtle[,1]"
    let turtleY r:get "turtle[,2]"
    r:eval "turtles <- rbind(turtles, turtle)"
  ]
  r:eval "spdf <- SpatialPointsDataFrame(turtles[1:2], turtles[3])"
  r:eval "homerange <- mcp(spdf, percent = 100, unin = c('km'), unout = c('km2'))"
  r:eval "hr.sizes <- as.data.frame(homerange)"
  r:eval "write.csv(hr.sizes, file='C:/NetLogo/Tiger_ABM/hr_sizes.csv')"

  ; create SpatialPointsDataFrame
  ask breeding-females with [count territory > 5]
  [
    set mcp r:get (word "hr.sizes$area[which(hr.sizes$id == '"name"')]")
  ]

  mark-homeranges
end

;-------------------------------------------------------------------------------------------------------------------------------------------

to mark-homeranges
  clear-drawing
  ;ask females with [age >= 24]

  ask breeding-females with [count territory > 5]
  [
    pen-up
    ; get the points of the homerange polygon for the current animal
    r:eval (word "temp <- slot(homerange,'polygons')[[which(slot(homerange,'data')$id == '"name"')]]@Polygons[[1]]@coords")
    let tempX r:get "temp[,1]"
    let tempY r:get "temp[,2]"
    let tempXY (map [ [?1 ?2] -> (list ?1 ?2) ] tempX tempY )
    ;graphics:fill-polygon tempXY

    ; create a turtle, which draws the homerange boundary
    hatch-homeranges 1
    [
      hide-turtle
      set name [name] of myself
      set color [color] of myself
    ]

    ; draw the homerange boundary
    foreach tempXY
    [ ?1 ->
      ask homeranges with [name = [name] of myself]
      [
        move-to patch (item 0 ?1) (item 1 ?1)
        set pen-size 2
        pen-down
      ]
    ]

    ; connect the last point of the homerange with the first one, to close the polygon
    ask homeranges with [name = [name] of myself]
    [
      let lastpoint first tempXY
      move-to patch (item 0 lastpoint) (item 1 lastpoint)
      pen-up
    ]
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
559
75
977
494
-1
-1
10.0
1
10
1
1
1
0
0
0
1
0
40
0
40
1
1
1
ticks
30.0

BUTTON
99
25
162
58
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
169
25
232
58
NIL
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
238
25
301
58
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
308
25
405
58
NIL
make-movie
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
35
340
252
373
terr-growth
terr-growth
1
36
12.0
1
1
NIL
HORIZONTAL

SLIDER
35
382
252
415
breed-male-surv
breed-male-surv
0
1
0.8
0.00001
1
NIL
HORIZONTAL

SWITCH
75
585
202
618
lists?
lists?
0
1
-1000

SWITCH
294
587
480
620
draw-MCP
draw-MCP
0
1
-1000

SLIDER
277
384
493
417
breed-fem-surv
breed-fem-surv
0
1
0.9
0.0001
1
NIL
HORIZONTAL

SLIDER
35
422
252
455
dis-male-surv
dis-male-surv
0
1
0.6496
.0001
1
NIL
HORIZONTAL

SLIDER
277
424
493
457
tran-fem-surv
tran-fem-surv
0
1
0.7
.0001
1
NIL
HORIZONTAL

SLIDER
35
462
252
495
juv-male-surv
juv-male-surv
0
1
0.9
0.0001
1
NIL
HORIZONTAL

SLIDER
277
464
493
497
juv-fem-surv
juv-fem-surv
0
1
0.9
.0001
1
NIL
HORIZONTAL

SLIDER
35
502
252
535
cub-male-surv
cub-male-surv
0
1
0.6
.0001
1
NIL
HORIZONTAL

SLIDER
277
504
493
537
cub-fem-surv
cub-fem-surv
0
1
0.6
0.0001
1
NIL
HORIZONTAL

SLIDER
35
543
252
576
tran-male-surv
tran-male-surv
0
1
0.65
0.0001
1
NIL
HORIZONTAL

SLIDER
35
70
252
103
tran-male-die-chall
tran-male-die-chall
0
1
0.25
0.0001
1
NIL
HORIZONTAL

SLIDER
277
109
492
142
breed-male-die-chall
breed-male-die-chall
0
1
0.6
.0001
1
NIL
HORIZONTAL

SLIDER
35
108
252
141
juv-die-infant
juv-die-infant
0
1
0.24
.0001
1
NIL
HORIZONTAL

SLIDER
277
72
492
105
cub-die-infant
cub-die-infant
0
1
0.79
.0001
1
NIL
HORIZONTAL

SLIDER
35
145
252
178
dom-take-subord
dom-take-subord
0
1
0.25
.0001
1
NIL
HORIZONTAL

SLIDER
35
260
252
293
max-wn-terr
max-wn-terr
0
200
167.3
.001
1
NIL
HORIZONTAL

SLIDER
277
225
493
258
min-wn-terr
min-wn-terr
0
100
76.0
.0001
1
NIL
HORIZONTAL

SLIDER
277
264
493
297
max-fem
max-fem
0
7
6.0
1
1
NIL
HORIZONTAL

SLIDER
35
184
252
217
fem-ideal-location
fem-ideal-location
0
10
8.0
1
1
NIL
HORIZONTAL

SLIDER
35
222
252
255
fem-marg-location
fem-marg-location
0
10
4.0
1
1
NIL
HORIZONTAL

SLIDER
277
185
493
218
male-search-rad
male-search-rad
0
15
12.0
1
1
NIL
HORIZONTAL

SLIDER
277
147
492
180
prop-prey-crop
prop-prey-crop
0
1
0.1
.0001
1
NIL
HORIZONTAL

SLIDER
277
544
493
577
fem-three-mate
fem-three-mate
0
1
0.9
0.0001
1
NIL
HORIZONTAL

SLIDER
277
344
493
377
fem-four-mate
fem-four-mate
0
1
1.0
0.0001
1
NIL
HORIZONTAL

SLIDER
35
299
252
332
fem-disp-rad
fem-disp-rad
50
200
132.0
.01
1
NIL
HORIZONTAL

SLIDER
277
300
493
333
male-disp-rad
male-disp-rad
50
400
264.0
.01
1
NIL
HORIZONTAL

SWITCH
75
624
202
657
view-terr?
view-terr?
1
1
-1000

SWITCH
294
625
481
658
hide-nonbreeders?
hide-nonbreeders?
1
1
-1000

SLIDER
67
697
239
730
num-of-fem
num-of-fem
1
4
4.0
1
1
NIL
HORIZONTAL

CHOOSER
303
691
475
736
prey-biomass
prey-biomass
"homogeneous" "random" "sm_random" "lr_gradient"
3

@#$#@#$#@
## WHAT IS IT?

This model simulates the territorial behaviour of different sizes of small tiger populations within landscapes with differing prey biomass patterns. Spatial distribution of prey biomass production affects the acquisition and development of female territories. Four types of prey biomass distribution proposed by Carter et al. (2015) are:

  1. **Homogeneous**. Prey biomass production of ~5 kg
  2. **Random**. Prey biomass production between 2.05 and 10.46 kg
  3. **Smoothed random**. A moving window passed over each cell to create slight gradient
  4. **Left-right gradient**. Gradient going from lowest(2.05 kg) to highest (10.46) prey biomass production


## HOW IT WORKS

This model simulates the behaviours of tigers, such as reproduction, mortality, dispersal, and resource selection (site fidelity). Most submodels were adapted from Carter et al. (2015), so you can refer to Carter et al. (2015) for the model description.

Differences between this model and the CNP_base model by Carter (2022):

  1. Size of the model landscape. _40x40_ cells
  2. Size of tiger population. Range (2-5)
  3. Spatial distribution of prey biomass production.

_Note_. Reproduction and mortality processes are turned off in this model.

## HOW TO USE IT

Most items in the Interface tab were copied from Carter et al. (2015) 's model, except for:

  1. _num-of-fem_, which represents the number of adult females created at the beginning of a simulation, and
2. _prey-biomass_, which represents the prey biomass pattern.

_Note_. No numerical output was expected from this model.

## THINGS TO NOTICE

Observe the visualisations of female territories for small tiger population sizes (1–4 female tigers), where they overlap locations with high prey density. Furthermore, the female territories either barely or completely did not overlap with those of their neighbours. These scenarios indicate that the model has successfully depicted the tigers' conquest for the best prey resources and their site fidelity behaviour. 

_Note._ This model focuses on how the various patterns of prey biomass spatial distributions influence the acquisition of territory for female tigers. Therefore, 100% MCPs depicting the boundaries of the territory will only be drawn for female tigers.


## EXTENDING THE MODEL

Try to figure out how to ensure that the female tigers will strictly acquire only the cells with the best prey abundance.


## NETLOGO FEATURES

This model utilises the R extension of NetLogo to calculate the home range size with the "adehabitatHR" package from the R software.

## CREDIT AND REFERENCES

Carter, N. (2022). CNP_base [Source code]. https://github.com/nhcarter/tiger_abm/blob/master/Tiger_base_model.zip
Carter, N., Levin, S., Barlow, A., & Grimm, V. (2015). Modeling tiger population and territory dynamics using an agent-based approach. _Ecological Modelling_, _312_, 347–362. https://doi.org/10.1016/j.ecolmodel.2015.06.008
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.2.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="240"/>
    <metric>count breeding-males + count breeding-females</metric>
    <metric>mean [hr-size] of breeding-males with [count females-in-my-territory &gt; 0]</metric>
    <metric>mean [hr-size] of breeding-females with [count territory &gt; 5]</metric>
    <metric>mean [count females-in-my-territory] of breeding-males with [count females-in-my-territory &gt; 0]</metric>
    <metric>mean [male-land-tenure] of breeding-males</metric>
  </experiment>
  <experiment name="experiment2" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="340"/>
    <metric>count cub-males</metric>
    <metric>count cub-females</metric>
    <metric>count cub-males + count cub-females</metric>
    <metric>count juvenile-females</metric>
    <metric>count juvenile-males</metric>
    <metric>count juvenile-females + count juvenile-males</metric>
    <metric>count transient-females</metric>
    <metric>count transient-males</metric>
    <metric>count transient-females + count transient-males</metric>
    <metric>count breeding-females</metric>
    <metric>count breeding-males</metric>
    <metric>count breeding-females + count breeding-males</metric>
    <metric>mean [hr-size] of breeding-males with [hr-size &gt; 0]</metric>
    <metric>mean [hr-size] of breeding-females with [hr-size &gt; 0]</metric>
    <metric>mean [male-land-tenure] of breeding-males</metric>
    <metric>mean [count females-in-my-territory] of breeding-males with [count females-in-my-territory &gt; 0]</metric>
  </experiment>
  <experiment name="my-terr-growth" repetitions="25" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>export-view "C:/Users/snurf/Desktop/export/view.png"</final>
    <timeLimit steps="12"/>
    <metric>count females + count males</metric>
    <enumeratedValueSet variable="terr-growth">
      <value value="12"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@

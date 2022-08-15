# Flow_Fields

Repository for making and testing out different types of flow fields. Some examples below, code for all outputs in [flow fields script](flow_field.R).

## Full flow fields

![flow with geom_path](./outputs/streams_02.png)
![flow with geom_segment](./outputs/streams.png)
![flow with light colours](./outputs/streams_03.png)
 
## Messing with parameters to make wave like flows

![flow like wave 1](./outputs/wave_02.png)
![flow like wave 2](./outputs/waves_01.png)

This one moves as a wave, and has a shadow 
![wave with shadow](./outputs/moving_wave.gif)

## Thinning out the flow field to it looks like paint strokes

![slimmed down flow with changing size](./outputs/paint_strokes.png)

## Making flow field gifs!
This one starts as a grid then *flows* away nicely

<img align="center" alt="from grid to flow" width="300px" src="https://raw.githubusercontent.com/andrewmoles2/Flow_Fields/main/outputs/running_to_streams.gif" />

## Removal of nearby lines
A fun technique is to remove any nearby lines/paths in the flow field. I've used knn for this, but I'm sure there are other ways of doing this! 

![example of using knn to remove nearby particles](./outputs/removal_demo.png)

This technique looks best when it has an image added behind it. Here I've added an image of horseshoe bend in the USA I took back in 2019. 

![add image behind removed flow](./outputs/horseshoe_bend.png)




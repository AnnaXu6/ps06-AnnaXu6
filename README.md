# ps06-AnnaXu6
### Your task is to build your own shiny application. The application should provide the user with the
### ability to interactively explore the dataset you chose. What the application looks like is up to you,
### but see the sample app https://otoomet.shinyapps.io/uah-lower-troposphere/.
### The visualization can be made with base R graphics, ggplot2, plotly, leaflet for maps or some-
### thing else. While we don’t expect you to create anything ground-breaking, we do expect you to
### create a clear visualization with proper labels and titles. And of course, the graphic needs to react
### to changing values in the widgets.
### Requirements:
### 1. You have at least three sets of pages (e.g. using tabsetPanel). One page (the opening page)
### displays the general information about the dataset, another page displays a plot, and the third
### page displays a table.
### 2. The plot and table page use either the sidebar layout, or another more complex layout (e.g.
### columns) that allows you to put the widgets and the rendered output next to each other.
### Just putting everything underneath each other is not OK.
### 3. The tab and plot page have a side panel in which you’ve created at least two widgets that
### change the visual output in your application, such as type of the plot, or data displayed on
### the xor yaxis of a scatterplot.
### Note: I mean a side panel but it does not have to be sidebarLayout.
### 4. The main panel of these pages displays data (as a plot or a table).
### 5. When you start the app, it should show explanatory text about your dataset and the app on
### the main panel. It may be very brief. No separate sidebars and no interactivity is needed
### here.
### 6. The explanatory text uses at least two html text formatting markers, such as strong(), em()
### or similar (raw html is OK too).
### See https://faculty.washington.edu/otoomet/info201-book/shiny.html#shiny-ui-text.
### 7. For both plot and table, there is at least one widget that allows the user to modify the values
### on screen. This may include filtering or some sort of computations. Just changing color is not
### enough.
### 8. The plot also includes a widget that allows one to change the visuals only, leaving data
### untouched.
### See https://faculty.washington.edu/otoomet/info201-book/shiny.html#shiny-server-reactive
### 9. The plot is appropriately labeled. This can be achieved either through ggplot() (or plot()
### if you wish) or through the layout elements.
### 10. Both plot and table page must also include some textual output that reacts to a widget, such
### as the number of non-missing observations, or the overall average value. For instance “Selected
### subset contains 1234 observations” is such a sentence, given 1234 is derived from the actual
### (subset of) data.
### 11. You must push your application up to the shinyapps.io server (or another server if you like),
### making it publicly available.
### 12. The uploaded should work on shinyapps, not crash!
### Note: this means you need to upload data, and ensure you use correct relative path!
### 13. Your github repo should include a readme file that contains a brief "user documentation" of
### your project: a brief description of the data, and explanation what are the widgets and panels
### doing. This file should contain a link to your project on the https://www.shinyapps.io
### server.
### 14. As submission, submit the link to your github repo to canvas.
### We hope that when you finish this project, you will good to go and start your final shiny
### applications!
# 2 Grading
### In total 100 points
### •(10pt) The app contains three different tabs or similar layout elements.
### •(15pt) the opening page
### – (5pt) Includes appropriate explanatory text
### – (5pt) Contains figures, calculated from data
### 2
### – (5pt) Contains HTML text formatting
### •(35pt) plot panel
### – (6pt) Contains sidebar and the main panel
### – (7pt) Sidebar contains a widget that modifies values
### – (8pt) Sidebar contains a widget the modifies visuals only
### – (7pt) Plot is appropriately labeled
### – (7pt) Plot includes a text summary that is computed of data (and changes reacts to the
### widget)
### •(20pt) table panel
### – (6pt) Contains sidebar and the main panel
### – (7pt) Sidebar contains a widget that changes data
### – (7pt) Table includes a text summary that is computed of data (and changes reacts to
### the widget)
### •(20pt) Publishing
### – Link available, app works as intended on shinyapps.io.
### Finally tell us how many hours did you spend on this PS.

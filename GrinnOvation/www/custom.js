Shiny.addCustomMessageHandler("sankey_plot_update", function(message) {
  Shiny.setInputValue("major_checkboxes", Shiny.inputBindings.bindingNames["shiny.checkboxGroupInput"].binding.getValue($("#major_checkboxes")[0]));
});
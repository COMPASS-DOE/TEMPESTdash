# Alerts module

# The module’s UI function
# The first argument to a UI function should always be `id`.
# This is the namespace for the module.
# https://shiny.posit.co/r/articles/improve/modules/
alertsUI <- function(id) {
    # `NS(id)` returns a namespace function
    ns <- NS(id)

    tabItem(
        tabName = "alerts",
        h3("Coming Soon!!")
        # textInput(inputId = "phone_number",
        #           label = "Phone number:",
        #           placeholder = "301-555-5555"),
        # selectInput(inputId = "carrier",
        #             label = "Select your carrier:",
        #             choices = c("Verizon", "AT&T", "T-Mobile", "Sprint")),
        # materialSwitch("system_alert", label = "System Alerts", status = "success", right = TRUE),
        # materialSwitch("value_alert", label = "Value Alerts", status = "success", right = TRUE),
        # # Only show this panel if Custom is selected
        # conditionalPanel(
        #     condition = "input.value_alert == 1",
        #     awesomeCheckboxGroup("value_alert_type",
        #                          label = "Choose dataset to receive alerts about:",
        #                          choices = c("Sapflow", "TEROS", "AquaTroll", "Battery"),
        #                          inline = TRUE)
        # ),
        # actionButton("txt_alert","Receive Text Alerts")
    )
}


# Module server function - a fragment of server logic
alertsServer <- function(id) {
    moduleServer(
        id,
        ## Below is the module function
        function(input, output, session) {

            # Coming soon!

        })
}

# Called by the server when the alertInvalidate timer fires
# Parameter dd is the dropbox_data() reactive from the server
send_alerts <- function(dd) {
    msg <- paste(
        paste0("System status as of: ",
               with_tz(Sys.time(), tzone = "America/New_York"), " EDT"),
        "",
        paste0("Sapflow: ", dd()$sapflow_bdg$percent_in),
        paste0("TEROS: ", dd()$teros_bdg$percent_in),
        paste0("Aquatroll: ", dd()$aquatroll_bdg$percent_in),
        paste0("Battery: ", dd()$battery_bdg$percent_in),
        sep = "\n")

    for(i in seq_len(nrow(TEXT_MSG_USERS))) {
        phone_number <- TEXT_MSG_USERS$number[i]
        carrier <- TEXT_MSG_USERS$carrier[i]

        carrier_email <- if(carrier == "Verizon") {
            carrier_email <- "@vtext.com"
        } else if(carrier == "AT&T") {
            carrier_email <- "@txt.att.net"
        } else if(carrier == "Sprint") {
            carrier_email <- "@messaging.sprintpcs.com"
        } else if(carrier == "T-Mobile") {
            carrier_email <- "@tmomail.net"
        }

        email <- paste0(phone_number, carrier_email)

        # Wrap this in a try so that if not authorized the app doesn't stop
        try({
            text_msg <- gm_mime() %>%
                gm_to(email) %>%
                gm_from("compassfme.tools@gmail.com") %>%
                gm_text_body(msg) # CHANGE THIS

            # need to add how often to send, right now only once

            gm_send_message(text_msg)
        })
    }

}

function resizeFrame() {

    var frame = window.frameElement;

    if (!frame) {
        console.log("No iframe detected");
        return;
    }

    var activeTab = document.querySelector('.tab-pane.active');

    var navbar = document.querySelector('.navbar');
    var navHeight = navbar ? navbar.offsetHeight : 0;

    var contentHeight;

    if (activeTab) {
        contentHeight = activeTab.getBoundingClientRect().height;

        // Include overflowing content
        contentHeight = Math.max(
            contentHeight,
            activeTab.scrollHeight,
            activeTab.offsetHeight
        );
    } else {
        contentHeight = document.documentElement.scrollHeight;
    }

    var height = Math.ceil(contentHeight + navHeight + 30);

    console.log(
        "Resizing iframe:",
        height,
        "px | tab:",
        activeTab
    );

    frame.style.height = height + "px";
    frame.style.minHeight = height + "px";
    frame.style.overflow = "hidden";

    document.documentElement.style.overflow = "hidden";
    document.body.style.overflow = "hidden";
}


/* ---------------------------------------------------------
   INITIAL LOAD
--------------------------------------------------------- */

window.addEventListener("load", function() {
    setTimeout(resizeFrame, 500);
    setTimeout(resizeFrame, 1500);
    setTimeout(resizeFrame, 3000);
});


/* ---------------------------------------------------------
   TAB CHANGES
--------------------------------------------------------- */

document.addEventListener("click", function(e) {

    var tab = e.target.closest(
        'a[data-bs-toggle="tab"], a[data-toggle="tab"]'
    );

    if (tab) {
        setTimeout(resizeFrame, 100);
        setTimeout(resizeFrame, 500);
        setTimeout(resizeFrame, 1500);
    }
});


/* ---------------------------------------------------------
   SHINY OUTPUTS
--------------------------------------------------------- */

$(document).on(
    "shiny:value shiny:outputinvalidated",
    function() {
        setTimeout(resizeFrame, 200);
        setTimeout(resizeFrame, 1000);
    }
);


/* ---------------------------------------------------------
   SHINY IDLE
--------------------------------------------------------- */

$(document).on("shiny:idle", function() {
    setTimeout(resizeFrame, 500);
});
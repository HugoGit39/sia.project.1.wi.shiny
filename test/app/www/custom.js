/* 1. App info*/

/* This function sets up the citation copy button. */

document.addEventListener("DOMContentLoaded", function () {
  const btn = document.getElementById("copy_citation_btn");
  if (btn) {
    btn.onclick = function () {
      const citation = `Schoenmakers M, Saygin M, Sikora M, Vaessen T, Noordzij M, de Geus E. Stress in action wearables database: A database of noninvasive wearable monitors with systematic technical, reliability, validity, and usability information. Behav Res Methods. 2025 May 13;57(6):171. doi: 10.3758/s13428-025-02685-4. PMID: 40360861; PMCID: PMC12075381.`;
      navigator.clipboard.writeText(citation).then(() => {
        alert("Citations copied to clipboard!");
      });
    };
  }
});

/* 2. Filters*/

// Product

$(document).ready(function () {

  setTimeout(function () {
    var button = $('.glossary-info-btn');
    var timer;

    button.popover({
      html: true,
      trigger: 'manual',
      placement: 'bottom',
      container: 'body',
      sanitize: false,
      template:
        '<div class="popover" role="tooltip" style="max-width:450px !important;">' +
        '<div class="arrow"></div><h3 class="popover-header"></h3><div class="popover-body"></div></div>',
      content: `
        <div id='glossary-popover-prod'
             style='max-height:240px; overflow-y:auto; overflow-x:hidden;
                    text-align:left; line-height:1.3; width:420px; padding-right:6px;'>
          <b>Table Information</b><br><br>

          <b>SiA Scores</b><br>
          • Long-Term SiA Score — Weighted technical, usability, and reliability score for extended use (>2 week).<br>
          • Short-Term SiA Score — Weighted technical and validation score for short-term use (≤2 week).<br><br>

          See the Research page for details on scoring, including criteria, procedure involved, and the interrater reliability.<br><br>

          <b>General Device Information</b><br>
          • Website — Link to the official device or manufacturer page.<br>
          • Release Year — Year of official device launch.<br>
          • Market Status — Current (available for purchase), Discontinued (no longer sold), or Upcoming (soon available).<br>
          • Main Use — Primary intended application (e.g., Research, Clinical, Consumer).<br>
          • Cost (€) — Approximate one-time purchase price including accessories.<br>
          • Type — Device form factor (e.g., watch, patch, chest strap, etc.).<br>
          • Location — Where the device is worn on the body (e.g., wrist, chest, arm).<br>
          • Size (mm) — Physical dimensions (length × width × height or diameter × height).<br>
          • Weight (gr) — Device mass in grams.<br><br>

          <b>Technical Specifications</b><br>
          • Bio Cueing — Feedback cues based on physiological signals (e.g., vibration or notification).<br>
          • Bio Feedback — Physiological parameters visible to user (e.g., stress or heart rate display).<br>
          • Water Resistance — Depth and/or time rating (e.g., 3 ATM, IP67).<br>
          • Battery Life (h) — Manufacturer-stated maximum hours of operation on one charge.<br>
          • Charging Duration (min) — Minutes required for full recharge.<br><br>

          <b>Signals</b><br>
          • Accelerometer — Measures movement or steps.<br>
          • Blood Pressure (BP) — Arterial pressure measurement.<br>
          • ECG — Electrocardiogram (electrical heart activity).<br>
          • EDA — Electrodermal Activity / Galvanic Skin Response.<br>
          • EEG — Electroencephalography (brain waves).<br>
          • EMG — Electromyography (muscle activity).<br>
          • GPS — Global Positioning System (geolocation).<br>
          • Gyroscope — Measures angular velocity and rotation.<br>
          • ICG — Impedance Cardiography (stroke volume, cardiac output).<br>
          • Other Signals — Additional measured signals not listed above.<br>
          • PPG — Photoplethysmography (blood volume pulse).<br>
          • Respiration — Derived from ICG or PPG signal variability.<br>
          • Skin Temperature — Peripheral temperature measured at skin surface.<br><br>

          <b>Data Access & Storage</b><br>
          • FDA Clearance — U.S. Food and Drug Administration medical approval.<br>
          • GDPR Compliance — Compliance with EU General Data Protection Regulation.<br>
          • CE Marking — European conformity mark for medical or consumer electronics.<br><br>
          • Internal Storage — Onboard memory available for signal storage.<br>
          • Raw Data Available — Whether signal-level data can be exported.<br>
          • Server Data Storage — Data stored on remote or manufacturer servers.<br>
          • Storage (hrs) — Approximate number of hours that can be stored locally.<br>
          • Storage (MB) — Approximate storage capacity in megabytes.<br><br>

          <b>Validation, Reliability & Usability</b><br>
          • Usability Studies (n) — Number of independent usability or adherence studies.<br>
          • Validity & Reliability Studies (n) — Number of studies validating signal quality or reliability.<br>
          • Usability Evidence Level — Evidence quality for usability (external > internal > none).<br>
          • Validity & Reliability Evidence Level — Evidence quality for validation and reliability (external > internal > none).<br><br>

          For full glossary, see sidebar&nbsp;<i class="fa fa-info-circle" style="color:#1c75bc;"></i>
        </div>
      `
    });

    // Show popover on hover
    button.on('mouseenter', function () {
      clearTimeout(timer);
      var btn = $(this);
      btn.popover('show');

      // Keep open when hovering the popover
      $(document).on('mouseenter', '#glossary-popover-prod', function () {
        clearTimeout(timer);
      });
      $(document).on('mouseleave', '#glossary-popover-prod', function () {
        timer = setTimeout(function () { btn.popover('hide'); }, 200);
      });
    });

    // Hide on mouseleave
    button.on('mouseleave', function () {
      var btn = $(this);
      timer = setTimeout(function () {
        if (!$('#glossary-popover-prod:hover').length) {
          btn.popover('hide');
        }
      }, 200);
    });
  }, 800);
});


// Feature

/* 3. Data*/

// Data submited
Shiny.addCustomMessageHandler("dataSubmitted", function(message) {
  alert(message);
});

/* 4. Research*/

/* 5. About*/

/* 6. Contact*/

// Email submited
Shiny.addCustomMessageHandler("emailSubmitted", function(message) {
  alert(message);
});












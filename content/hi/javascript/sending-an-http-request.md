---
title:                "Hindi में एक http अनुरोध भेजना"
html_title:           "Javascript: Hindi में एक http अनुरोध भेजना"
simple_title:         "Hindi में एक http अनुरोध भेजना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
एचटीटीपी अनुरोध भेजना एक आम प्रोग्रामिंग अभिविन्यास है, जिसका उपयोग दूसरे सर्वर से डेटा अनुरोध करने के लिए किया जाता है। प्रोग्रामर इसका उपयोग करते हैं क्योंकि यह नेटवर्क कम्यूनिकेशन को सरल और आसान बनाता है और अतिरिक्त उपस्थिति के बिना डेटा मिल सकता है।

## कैसे करें:
```Javascript 
let xhttp = new XMLHttpRequest();
xhttp.onreadystatechange = function() {
  if (this.readyState == 4 && this.status == 200) {
    console.log(this.responseText);
  }
};
xhttp.open("GET", "https://example.com", true);
xhttp.send();
```

कोड की यह उदाहरण स्थानांतरित डेटा प्राप्त करने के लिए गूगल को अनुरोध करता है। अनुरोध के उत्तर में, यह कोनसीयएटीपी के साथ अद्यतन करके, उत्तरको लॉग में दिखाता है।
```
<!DOCTYPE html>
<html>
<body>

<h2>Testing</h2>
<script>
let xhttp = new XMLHttpRequest();
xhttp.onreadystatechange = function() {
  if (this.readyState == 4 && this.status == 200) {
    document.getElementById("demo").innerHTML = this.responseText;
  }
};
xhttp.open("GET", "https://example.com", true);
xhttp.send();
</script>
<p id="demo"></p>

</body>
</html>
```

## गहराई में जाएं:
एचटीटीपी रिक्वेस्ट का प्रयोग करने के पीछे एक ऐतिहासिक पृष्ठभूमि है, जहां प्रदान कर्ता और विनिमय डेटा को पेश करने के लिए उत्पादनी मानक तंत्र तैनात किया गया था। आज, अन्य विकल्प जैसे कि फीचर फोन और इंटरनेट संबंध फोन भी हैं जो यह काम कर सकते हैं। भविष्य में, इंटरनेट ऑफ थिंग्स (आईओटी) भी एचटीटीपी रिक्वेस्ट को पेश करने के लिए उपयुक्त हो सकता है। इसके अलावा, एचटीटीपी रिक्वेस्ट की विस्तृत विश्लेषण उदाहरण के साथ उपलब्ध है।

## अन्य संबंधित स्रोत:
- [जावास्क्रिप्ट डॉक्यूमेंटेशन](https://developer.mozilla.org/hi/docs/Web/API/XMLHttpRequest)
- [विकिपीडिया](https://hi.wikipedia.org/wiki/XMLHttpRequest)
- [एक्सएमएलएन टोयो स्क्रिप्टिंग स्क्कुल](https://www.w3schools.com/js/js_ajax_xmlhttprequest_send.asp)
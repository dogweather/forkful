---
title:                "HTML का विश्लेषण"
html_title:           "Gleam: HTML का विश्लेषण"
simple_title:         "HTML का विश्लेषण"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/parsing-html.md"
---

{{< edit_this_page >}}

## क्यों

आपको शायद ही पता हो कि वेब के हर पृष्ठ में होते हैं उस्ताद पाठ, जिन्हें आपको समझने के लिए HTML को पेशे वार ढंग से पार्स किया जाना होगा। एक उच्च गुणवत्ता वाले पार्शिंग टूल के बिना, यह काम थोड़ा मुश्किल हो सकता है। इसलिए, हमारे पाठकों के लिए, हम बताने जा रहे हैं कि ग्लीम कैसे आपकी मदद कर सकता है HTML को पार्स करने में।

## कैसे करें

```gleam
import html
parsed_html = html.parse("""<html>
<head>
  <title>Gleam Tutorial</title>
</head>
<body>
  <h1>Welcome to Gleam</h1>
  <p>This is a tutorial about parsing HTML with Gleam.</p>
</body>
</html>""")
```

जैसा कि आप ऊपर दिखा रहे कोड से देख सकते हैं, हमने पहले ग्लीम की `html` मॉड्यूल को आयात किया और फिर हमें एक HTML स्ट्रिंग को पार्स करने के लिए `html.parse` फंक्शन को बुलाया। हमारा आउटपुट `parsed_html` नाम के एक ओब्जेक्ट के अंदर स्टोर होगा जिसे हम उपयोग करके HTML के अलग-अलग एलिमेंट्स, एट्रिब्यूट्स और टेग से एकसाथ अलग डेटा को एक्सेस कर सकते हैं।

```gleam
title = parsed_html.find("title")
p = parsed_html.find(".//p")
```

ऊपर दिए हुए उदाहरण से आप देख सकते हैं कि हम एक `title` एलिमेंट को स्ट्रिंग के रूप में `title` नामक भेदक की सहायता से एक्सेस कर सकते हैं और वहीं हम `find` फंक्शन का उपयोग करके परिणाम से `p` को एक पैराग्राफ़ एलिमेंट के रूप में हासिल कर सकते हैं।

```gleam
print(title.text) // Output: Gleam Tutorial
print(p.text) // Output: This is a tutorial about parsing HTML with Gleam.
```

अधिक उपयोगी फंक्शनों के बारे में
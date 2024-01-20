---
title:                "एक अस्थायी फ़ाइल बनाना"
html_title:           "Arduino: एक अस्थायी फ़ाइल बनाना"
simple_title:         "एक अस्थायी फ़ाइल बनाना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
तात्कालिक फ़ाइल बनाना एक ऐसी प्रक्रिया है जिसमें क्रिएट किया गया डाटा अस्थायी रूप से संग्रहीत होता है। प्रोग्रामर्स इसे डाटा का अस्थायी प्रवाहन, कंफिगरेशन सेटिंग्स, और डिबग करने के लिए करते हैं।

## कैसे करें:
यहां ग्लीम (Gleam) कोडिंग के कुछ उदाहरण:

```Gleam
fn temporary_file() {
  let temp_dir = std.os.temp_dir();
  let (file, path) = std.io.new_temp_file(temp_dir, "gleam-temp-file").unwrap();
  
  println!("नई अस्थायी फ़ाइल {} बनाई गई है", path.display());
}
```

यहां 'gleam-temp-file' नाम से एक नई अस्थायी फ़ाइल बनती है। आउटपुट：
```
नई अस्थायी फ़ाइल /tmp/gleam-temp-file-4fg1d बनाई गई है
```

## गहरी झांकी
1) हिस्टोरिकल कांटेक्स्ट: UNIX ऑपरेटिंग सिस्टम के प्रारंभिक दिवसों से अस्थायी फ़ाइलें उपयोगी प्रमाणित हैं। यह सिस्टम को भरोसेमंद रूप से चलाने और क्रैश से डाटा की सुरक्षा करने में सहायता करती हैं।
2) विकल्प: फ़ाइल सिस्टम की जगह मेमोरी-आधारित विकल्प जैसे रैम डिस्क भी उपयोगी हो सकते हैं। इससे परफॉरमेंस बेहतर होती है, लेकिन आपा मेमोरी की सीमितताओं का सामना करने की संभावना भी होती है। 
3) आवश्यकताएँ: अस्थायी फ़ाइलें अगर किसी सदस्य को विशेष अनुमतियाँ दी जाती हैं, यह सुनिश्चित करती हैं कि कोई अनधिकृत उपयोग नहीं हो सकता।

## और भी देखें
1) ["अस्थायी फ़ाइलें में UNIX और Linux"](https://www.geekhideout.com/tempfiles.shtml)
2) ["Gleam प्रोग्रामिंग भाषा की अधिक जानकारी"](https://gleam.run/tour/)
3) ["अस्थायी फ़ाइलें बनाने का Secure तरीका"](https://www.ietf.org/rfc/rfc3779.txt)
4) ["Gleam और अन्य भाषाओं की तुलना"](https://gleam.run/news/gleam-v0.8-released/)
---
title:                "एक अस्थायी फ़ाइल बनाना"
html_title:           "Go: एक अस्थायी फ़ाइल बनाना"
simple_title:         "एक अस्थायी फ़ाइल बनाना"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## क्यों

टेम्पररी फ़ाइल बनाने का काम आपको अपनी आवश्यकता के अनुसार अपने कोड में अस्थायी फाइलों को बनाने की अनुमति देता है। इससे आपको अनुभवी और सुरक्षित कोड के साथ काम करने में मदद मिलती है।

## कैसे करें

```Go
package main

import (
	"fmt"
	"io/ioutil"
	"os"
)

func main() {
	// temporary file को लिखने के लिए "ioutil" पैकेज का उपयोग करें
	tempFile, err := ioutil.TempFile("", "example")
	if err != nil {
		panic(err)
	}

	// विभिन्न डेटा टेम्पररी फ़ाइल में लिखें
	defer os.Remove(tempFile.Name())
	fmt.Println("टेम्पररी फ़ाइल का नाम:", tempFile.Name())
	fmt.Fprintln(tempFile, "यह टेम्पररी फ़ाइल है!")
}
```

आउटपुट:

```
टेम्पररी फ़ाइल का नाम: /var/folders/rm/rzqn_zsn4tldghg_d6jvkc9c_0000gn/T/example564444140
```

आप देख सकते हैं कि टेम्पररी फ़ाइल का नाम सिस्टम के साथ आपकी कार्य स्थान के डिरेक्टरी में एक अनूठा नाम प्रदान किया गया है। आप उस फ़ाइल को साफ़ कर सकते हैं जब आपको उसकी आवश्यकता खत्म हो जाए।

## गहरा ज्ञान (Deep Dive)

टेम्पररी फ़ाइल उत्पन्न करने के लिए, आपको सबसे पहले `ioutil.TempFile()` फ़ंक्शन को उपयोग करना होगा। इस फ़ंक्शन में आपको दो पैरामीटर पास करने होंगे - पहले में स्थायी या अस्थायी फ़ाइलों को सेव करने के लिए डिरेक्टरी का पथ दिया गया होना चाहिए और दूसरे पर आपको उस फ़ाइल का प्रथम नाम देना होगा जो उत्पन्न होने जा रहा है।

आप `ioutil.TempDir()` फ़ंक्शन को उपयोग करके
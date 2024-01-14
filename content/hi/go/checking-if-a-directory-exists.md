---
title:                "Go: डायरेक्टरी मौजूद है की नहीं जांचें"
simple_title:         "डायरेक्टरी मौजूद है की नहीं जांचें"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## क्यों

एक डायरेक्टरी का अस्तित्व जाँचना उपयोगी हो सकता है जब आपको अपने प्रोग्राम में ऑपरेशन करने से पहले सुनिश्चित करना हो कि वह मौजूद है या नहीं। अस्तित्व जाँचना आपको अनचाही त्रुटियों और बगों से बचाकर समय और प्रयास की बचत कर सकता है।

## कैसे 

```Go
if _, err := os.Stat("/path/to/directory"); os.IsNotExist(err) {
    fmt.Println("Directory does not exist")
} else {
    fmt.Println("Directory exists")
}
```

जब आपको डायरेक्टरी का अस्तित्व जाँचना होता है, आप `os.Stat()` फ़ंक्शन का उपयोग कर सकते हैं जो एक `os.FileInfo` ऑब्जेक्ट वापस करता है। यदि फ़ाइल या डायरेक्टरी मौजूद नहीं है, तो एक त्रुटि वापस की जाएगी जो `os.IsNotExist(err)` के माध्यम से जाँची जा सकती है। 

## डीप डाइव

यहाँ कुछ और विस्तृत जानकारी है जो आपको समझने में मदद कर सकती है कि डायरेक्टरी का अस्तित्व कैसे जाँचा जाता है। इस प्रक्रिया में आप `os.IsNotExist()` फ़ंक्शन के साथ सामान्य त्रुटियों के लिए उपयोग किया गया है, जो `os.Stat()` फ़ंक्शन के साथ संबंधित हैं। आप भी `os.IsPermission()` फ़ंक्शन का उपयोग कर सकते हैं जो डायरेक्टरी या फ़ाइल तक पहुंच की अनुमति से संबंधित त्रुटियों को दर्शाता है।

## देखें भी

- [Go डाकतार के ऑफिशियल डॉक्युमेंटेशन](https://golang.org/pkg/os/#Stat)
- [Tutorialspoint पर Go में फ़ाइलों और डायरेक्टरियों का अस्तित्व जाँचना](https://www.tutorialspoint.com/go/go_file_existence.htm)
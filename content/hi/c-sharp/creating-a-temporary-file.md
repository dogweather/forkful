---
title:                "C#: अस्थायी फाइल बनाना"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## क्यों

एक अस्थायी फ़ाइल बनाने में लोगों का आदेश क्यों हो सकता है, उसके बारे में कुछ उपयोगी सूचनाएँ हैं।

## कैसे

अस्थायी फ़ाइल बनाने के लिए, हम `Path.GetTempFileName()` फ़ंक्शन का उपयोग कर सकते हैं, जो फ़ाइल नाम और पथ दोनों को वापस करता है। नीचे दिये गए कोड ब्लॉक एक उदाहरण प्रदान करते हैं:

```C#
string tempFilePath = Path.GetTempFileName();
Console.WriteLine("Temporary file created: " + tempFilePath);

// Output: Temporary file created: C:\Users\Username\AppData\Local\Temp\af7a28ba.tmp
```

यदि आप अपनी स्वयं की अस्थायी फ़ाइल बनाना चाहते हैं, तो आप निम्नलिखित कोड का उपयोग कर सकते हैं:

```C#
string tempFileName = "myTemporaryFile.txt";
string tempFilePath = Path.Combine(Path.GetTempPath(), tempFileName); // combining temp path and file name
File.Create(tempFilePath); // creates a new temporary file with specified file name
Console.WriteLine("Temporary file created: " + tempFileName);

// Output: Temporary file created: myTemporaryFile.txt
```

## घोर तौर पर पता करें

स्वयं की अस्थायी फ़ाइल बनाने के लिए, आपको ध्यान देने की जरूरत है कि आपको निर्दिष्ट गति के साथ फ़ाइल नाम देना होगा ताकि वह दो उपलब्ध फ़ाइलों के बीच अलग हो सकें। आपको हर बार वही गति या उससे अधिक वाली को उपयोग करना होगा जो प्रत्येक प्रक्रिया में अलग हो सकेगी। इसके अलावा, अस्थायी फ़ाइलें प्रत्येक समय ट्रिगर होने या पूरा प्रक्रिया के साथ सही समय पर हटाई जाने के लिए निर्दिष्ट किए गए नहीं होती हैं। प्रकार सार्वजनिक और प्रत्येक कर्म द्वारा समय सीमा का उपयोग किए बिना अस्थायी फ़ाइलों को हटाने के लिए ज
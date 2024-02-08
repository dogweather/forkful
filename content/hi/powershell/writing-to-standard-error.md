---
title:                "मानक त्रुटि के लिए लिखना"
date:                  2024-02-03T19:35:15.049183-07:00
model:                 gpt-4-0125-preview
simple_title:         "मानक त्रुटि के लिए लिखना"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

PowerShell में स्टैंडर्ड एरर (stderr) में लेखन का मतलब एरर संदेशों या डायग्नोस्टिक्स को सीधे stderr स्ट्रीम पर भेजना होता है, जो कि स्टैंडर्ड आउटपुट (stdout) स्ट्रीम से अलग होती है। इस पृथक्करण के द्वारा, स्क्रिप्ट के आउटपुट पर अधिक सटीक नियंत्रण संभव होता है, जो विकसकों को सामान्य और एरर संदेशों को विभिन्न लक्ष्यों पर निर्देशित करने में सक्षम बनाता है, जो कि एरर हैंडलिंग और लॉगिंग के लिए मौलिक है।

## कैसे करें:

PowerShell stderr में लिखने की प्रक्रिया को `Write-Error` cmdlet का उपयोग करके या आउटपुट को `$host.ui.WriteErrorLine()` विधि पर निर्देशित करके सरल बनाता है। हालांकि, सीधे stderr रीडायरेक्शन के लिए, आप .NET विधियों का उपयोग करना पसंद कर सकते हैं या PowerShell द्वारा प्रस्तावित फाइल डिस्क्रिप्टर रीडायरेक्शन।

**उदाहरण १:** `Write-Error` का उपयोग करके stderr पर एक एरर संदेश लिखना।

```powershell
Write-Error "This is an error message."
```

stderr पर आउटपुट:
```
Write-Error: This is an error message.
```

**उदाहरण २:** `$host.ui.WriteErrorLine()` का उपयोग करके सीधे stderr लेखन।

```powershell
$host.ui.WriteErrorLine("Direct stderr write.")
```

stderr पर आउटपुट:
```
Direct stderr write.
```

**उदाहरण ३:** .NET विधियों का उपयोग करके stderr में लेखन।

```powershell
[Console]::Error.WriteLine("Using .NET method for stderr")
```

इस विधि का आउटपुट:
```
Using .NET method for stderr
```

**उदाहरण ४:** फाइल डिस्क्रिप्टर `2>` का उपयोग करके एरर आउटपुट रीडायरेक्ट करना।

PowerShell में फाइल डिस्क्रिप्टर विभिन्न स्ट्रीम्स को रीडायरेक्ट कर सकते हैं। stderr के लिए, फाइल डिस्क्रिप्टर `2` होता है। यहाँ एक उदाहरण है जिसमें एक एरर उत्पन्न करने वाले कमांड को निष्पादित करते समय stderr को `error.log` नामक फाइल में रीडायरेक्ट करना है।

```powershell
Get-Item NonExistentFile.txt 2> error.log
```

इस उदाहरण से कोई कंसोल आउटपुट नहीं उत्पन्न होता, लेकिन वर्तमान निर्देशिका में एक फाइल `error.log` उत्पन्न होती है जिसमें उस फाइल तक पहुँचने के प्रयास में आने वाले एरर संदेश को संग्रहित किया जाता है जो मौजूद नहीं है।

निष्कर्ष में, PowerShell एरर आउटपुट को प्रभावी रूप से लिखने और प्रबंधित करने के लिए कई विधियाँ प्रदान करता है, जो स्क्रिप्ट्स और अनुप्रयोगों में उन्नत एरर हैंडलिंग और लॉगिंग रणनीतियों को सक्षम बनाता है।

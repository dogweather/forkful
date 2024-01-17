---
title:                "एक पाठ फ़ाइल लिखना"
html_title:           "PowerShell: एक पाठ फ़ाइल लिखना"
simple_title:         "एक पाठ फ़ाइल लिखना"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
टेक्स्ट फ़ाइल लेखन में क्या चीज है और क्यों प्रोग्रामर इसे करते हैं? टेक्स्ट फ़ाइल लेखन से आप अपने पाठ्यक्रम में अतिरिक्त जानकारी बचाएंगे और अपने स्क्रिप्ट्स में उपयोगी डेटा को बचाएंगे।

## कैसे करें:
जब आप एक टेक्स्ट फ़ाइल लिखना चाहते हैं, तो आपको PowerShell में "Set-Content" कमांड का उपयोग करना होगा। नीचे दिए गए उदाहरण में, हम स्ट्रिंग कोई बिल्कुल नयी खाली टेक्स्ट फ़ाइल में लिखने का उपयोग करेंगे।

```PowerShell
Set-Content -Path C:\Users\Username\Documents\sample.txt -Value "Hello world!"
```

और यदि आपने पहले से ही एक टेक्स्ट फ़ाइल बना ली है, तो आप उस फ़ाइल मेंं अपने नए टेक्स्ट को जोड़ सकते हैं।

```PowerShell
Add-Content -Path C:\Users\Username\Documents\sample.txt -Value "This is new text."
```

आप भी एक फ़ोल्डर्स्रच को चुन सकते हैं, जहाँ आप एक नई फ़ाइल बनाना चाहते हैं।

```PowerShell
New-Item -Path C:\Users\Username\Documents\ -Name "new_sample.txt" -ItemType "file" -Value "This is a new text file."
```

## गहराई में जाइए:
टेक्स्ट फ़ाइल लेखन का इतिहास बहुत पुराना है और यह प्रोग्रामिंग के कई अन्य विषयों के संबंध में भी जानना जरूरी है। आप अपने स्क्रिप्ट में अतिरिक्त जानकारी बचा सकते हैं और भविष्य में उसे सुलभ बना सकते हैं एक अलग टेक्स्ट फाइल में। अन्य विकल्पों में "Out-File" कमांड हो सकता है जो एक फाइल को नया बना सकता है या "Add-Content" को उस फाइल मेंं जोड़ सकता है।

## आपको भी देखना चाहिए:
- [PowerShell documentation](https://docs.microsoft.com/en-us/powershell/scripting)
- [Scripting basics](https://www.advancedinstaller.com/user-guide/tutorial-powershell-script.html)
- [Tutorial on writing text files with PowerShell](https://www.joseespitia.com/2017/04/16/powershell-games-write-files-writing-text-files/)
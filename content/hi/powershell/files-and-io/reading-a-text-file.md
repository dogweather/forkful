---
date: 2024-01-20 17:55:21.797471-07:00
description: "How to (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902): \u092A\u093E\
  \u0920 \u092B\u093E\u0907\u0932 \u092A\u0922\u093C\u0928\u0947 \u0915\u093E \u0915\
  \u093E\u092E `Get-Content` cmdlet \u0915\u0947 \u0938\u093E\u0925 \u0906\u0938\u093E\
  \u0928\u0940 \u0938\u0947 \u0939\u094B \u091C\u093E\u0924\u093E \u0939\u0948, \u091C\
  \u094B PowerShell \u092E\u0947\u0902 \u0936\u0941\u0930\u0941\u0906\u0924 \u0938\
  \u0947 \u0939\u0940 \u0936\u093E\u092E\u093F\u0932 \u0939\u0948\u0964 \u092A\u093E\
  \u0920 \u092B\u093C\u093E\u0907\u0932 \u0915\u094B \u092A\u0922\u093C\u0928\u0947\
  \ \u0915\u0947\u2026"
lastmod: '2024-04-05T22:51:07.398475-06:00'
model: gpt-4-1106-preview
summary: "\u092A\u093E\u0920 \u092B\u093E\u0907\u0932 \u092A\u0922\u093C\u0928\u0947\
  \ \u0915\u093E \u0915\u093E\u092E `Get-Content` cmdlet \u0915\u0947 \u0938\u093E\
  \u0925 \u0906\u0938\u093E\u0928\u0940 \u0938\u0947 \u0939\u094B \u091C\u093E\u0924\
  \u093E \u0939\u0948, \u091C\u094B PowerShell \u092E\u0947\u0902 \u0936\u0941\u0930\
  \u0941\u0906\u0924 \u0938\u0947 \u0939\u0940 \u0936\u093E\u092E\u093F\u0932 \u0939\
  \u0948\u0964 \u092A\u093E\u0920 \u092B\u093C\u093E\u0907\u0932 \u0915\u094B \u092A\
  \u0922\u093C\u0928\u0947 \u0915\u0947 \u0935\u093F\u0915\u0932\u094D\u092A\u094B\
  \u0902 \u092E\u0947\u0902 `[System.IO.File]` \u0928\u0947\u092E\u0938\u094D\u092A\
  \u0947\u0938 \u0915\u0947 \u0924\u0939\u0924 .NET \u0915\u094D\u0932\u093E\u0938\
  \u0947\u0938 \u0915\u093E \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0936\
  \u093E\u092E\u093F\u0932 \u0939\u0948\u0964 \u092F\u0939 \u0906\u092A\u0915\u094B\
  \ \u092C\u0921\u093C\u0940 \u092B\u093E\u0907\u0932\u094B\u0902 \u0915\u094B \u0905\
  \u0927\u093F\u0915 \u0915\u0941\u0936\u0932\u0924\u093E \u0938\u0947 \u092A\u0922\
  \u093C\u0928\u0947 \u0914\u0930 \u0909\u0928 \u092A\u0930 \u0915\u093E\u092E \u0915\
  \u0930\u0928\u0947 \u092E\u0947\u0902 \u092E\u0926\u0926 \u0915\u0930\u0924\u093E\
  \ \u0939\u0948\u0964 \u0938\u094D\u091F\u094D\u0930\u0940\u092E\u093F\u0902\u0917\
  \ \u0924\u092C \u0909\u092A\u092F\u0941\u0915\u094D\u0924 \u0939\u094B\u0924\u0940\
  \ \u0939\u0948 \u091C\u092C \u0906\u092A\u0915\u094B \u092E\u0947\u092E\u094B\u0930\
  \u0940 \u0915\u093E \u0916\u094D\u092F\u093E\u0932 \u0930\u0916\u0928\u093E \u0939\
  \u094B\u0964."
title: "\u091F\u0947\u0915\u094D\u0938\u094D\u091F \u092B\u093C\u093E\u0907\u0932\
  \ \u092A\u0922\u093C\u0928\u093E"
weight: 22
---

## How to (कैसे करें):
```PowerShell
# टेक्स्ट फाइल पढ़ने का सीधा तरीका
$textContent = Get-Content 'C:\path\to\your\file.txt'
$textContent

# पंक्तियों को व्यक्तिगत रूप से पढ़ना
$lineByLine = Get-Content 'C:\path\to\your\file.txt' -ReadCount 1
foreach ($line in $lineByLine) {
    $line
}

# एक बड़ी फाइल को स्ट्रीमिंग के साथ पढ़ना
$reader = [System.IO.File]::OpenText('C:\path\to\your\largeFile.txt')
try {
    while ($null -ne ($line = $reader.ReadLine())) {
        $line
    }
}
finally {
    $reader.Close()
}
```

सैंपल आउटपुट:
```
नमस्कार, यह पहली पंक्ति है।
और यह दूसरी पंक्ति।
```

## Deep Dive (गहराई से जानकारी):
पाठ फाइल पढ़ने का काम `Get-Content` cmdlet के साथ आसानी से हो जाता है, जो PowerShell में शुरुआत से ही शामिल है। पाठ फ़ाइल को पढ़ने के विकल्पों में `[System.IO.File]` नेमस्पेस के तहत .NET क्लासेस का इस्तेमाल शामिल है। यह आपको बड़ी फाइलों को अधिक कुशलता से पढ़ने और उन पर काम करने में मदद करता है। स्ट्रीमिंग तब उपयुक्त होती है जब आपको मेमोरी का ख्याल रखना हो।

## See Also (और देखें):
- PowerShell `[System.IO.File]` क्लास: https://docs.microsoft.com/en-us/dotnet/api/system.io.file
- `Get-Content` Cmdlet डॉक्यूमेंटेशन: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/get-content

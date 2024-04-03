---
date: 2024-01-20 17:55:21.797471-07:00
description: "How to (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902): ."
lastmod: '2024-03-13T22:44:52.732633-06:00'
model: gpt-4-1106-preview
summary: .
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

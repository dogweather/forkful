---
title:                "डीबग आउटपुट प्रिंट करना"
aliases:
- hi/ruby/printing-debug-output.md
date:                  2024-01-20T17:54:07.077621-07:00
model:                 gpt-4-1106-preview
simple_title:         "डीबग आउटपुट प्रिंट करना"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
Debug output मतलब अपने प्रोग्राम के चलते हुए इंटरनल जानकारियों को प्रिंट करना। प्रोग्रामर्स इसे इसलिए करते हैं ताकि वो अपने कोड के बिहेवियर को समझ सकें और गलतियों को ढूँढ सकें।

## How to: (कैसे?)
```Ruby
# सिंपल मैसेज print करने के लिए
puts "Debugging: वैल्यू देखने के लिए"

# वैरिएबल की वैल्यू print करना
number = 42
puts "Debugging: number की वैल्यू #{number}"

# उदाहरण आउटपुट:
# Debugging: वैल्यू देखने के लिए
# Debugging: number की वैल्यू 42
```

## Deep Dive (गहराई से जानकारी)
डिबग प्रिंटिंग की शुरुआत हुई थी कोडिंग के शुरवाती दौर में, जब लॉग फाइल्स और स्पेशल डिबगिंग टूल्स नहीं होते थे। आज भी, puts का इस्तेमाल सबसे ज्यादा है। हालांकि, Ruby में `p` (जो .inspect को कॉल करता है) और `pp` (pretty print) जैसे अल्टरनेटिव्स भी हैं।

कभी-कभी, `puts` और `p` का इस्तेमाल नुकसान देह हो सकता है, खासकर बड़े प्रोग्राम्स में जहां यह जानकारी को ढूंढना मुश्किल बनाते हैं। ऐसे में, `Logger` क्लास का प्रयोग किया जा सकता है जो जानकारी को फाइल्स में स्टोर करती है, और इसे अलग-अलग स्तरों (info, warn, error) में सेग्रिगेट करती है।

```Ruby
require 'logger'

logger = Logger.new(STDOUT)
logger.info("Main में वैल्यू बदल गयी है")
logger.debug("number नाम के वैरिएबल की वैल्यू: #{number}")
```

आप आवश्यकता अनुसार `Logger` की ग्रेनुलारिटी और फॉरमेट को कस्टमाइज़ भी कर सकते हैं।

## See Also (और भी जानकारी)
- Ruby's Logger Class: [Ruby-Doc.org](https://ruby-doc.org/stdlib-2.4.0/libdoc/logger/rdoc/Logger.html)
- `puts` vs `p` vs `print` in Ruby: [Stack Overflow](https://stackoverflow.com/questions/5100299/whats-the-difference-between-puts-and-print-in-ruby)

---
title:                "स्ट्रिंग इंटरपोलेशन"
date:                  2024-01-20T17:51:22.180286-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग इंटरपोलेशन"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

String interpolation एक तरीका है जिसमें हम PHP variables को सीधे strings के अंदर डालते हैं. इससे कोड पढ़ने और लिखने में आसानी होती है क्योंकि यह variable values को string के साथ जोड़ने का एक short और clear तरीका है.

## How to: (कैसे करें:)

```PHP
$name = 'अंकित';
$greeting = "नमस्ते, $name! कैसे हैं आप?";

echo $greeting; // यह print करेगा: नमस्ते, अंकित! कैसे हैं आप?
```
Complex expression के लिए curly braces का इस्तेमाल:

```PHP
$order = (object) ['number' => 123];
echo "आपका ऑर्डर नंबर { $order->number } प्रोसेस में है.";

// यह print करेगा: आपका ऑर्डर नंबर 123 प्रोसेस में है.
```

## Deep Dive (गहराई से जानकारी):

String interpolation पहली बार PHP में version 3 में आया था. हम double quotes ("") या heredoc syntax का इस्तेमाल कर string में variable डाल सकते हैं. Single quotes ('') में यह काम नहीं करता.

Alternatives में concatenation (.) operator है, जो variables को strings के साथ जोड़ने के लिए use होता है, लेकिन यह थोड़ा clumsy हो सकता है:
```PHP
echo 'नमस्ते, ' . $name . '! कैसे हैं आप?';
```

Implementation details:
- Complex expression जैसे object properties या array elements को interpolate करने के लिए `{}` का use करना पड़ता है.
- Interpolation करते समय PHP यह check करता है कि variable exist करता है और non-null है.
- Performance के perspective से, string interpolation generally concatenation से तेज़ होता है.

## See Also (और जानकारी के लिए:)

- PHP documentation on string interpolation: [php.net/manual/en/language.types.string.php#language.types.string.parsing](https://www.php.net/manual/en/language.types.string.php#language.types.string.parsing)
- PHP string functions: [php.net/manual/en/ref.strings.php](https://www.php.net/manual/en/ref.strings.php)
- PHP string operators: [php.net/manual/en/language.operators.string.php](https://www.php.net/manual/en/language.operators.string.php)

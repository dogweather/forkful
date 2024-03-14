---
date: 2024-01-20 17:51:22.180286-07:00
description: "String interpolation \u090F\u0915 \u0924\u0930\u0940\u0915\u093E \u0939\
  \u0948 \u091C\u093F\u0938\u092E\u0947\u0902 \u0939\u092E PHP variables \u0915\u094B\
  \ \u0938\u0940\u0927\u0947 strings \u0915\u0947 \u0905\u0902\u0926\u0930 \u0921\u093E\
  \u0932\u0924\u0947 \u0939\u0948\u0902. \u0907\u0938\u0938\u0947 \u0915\u094B\u0921\
  \ \u092A\u0922\u093C\u0928\u0947 \u0914\u0930 \u0932\u093F\u0916\u0928\u0947 \u092E\
  \u0947\u0902 \u0906\u0938\u093E\u0928\u0940 \u0939\u094B\u0924\u0940 \u0939\u0948\
  \ \u0915\u094D\u092F\u094B\u0902\u0915\u093F \u092F\u0939 variable\u2026"
lastmod: '2024-03-13T22:44:52.453078-06:00'
model: gpt-4-1106-preview
summary: "String interpolation \u090F\u0915 \u0924\u0930\u0940\u0915\u093E \u0939\u0948\
  \ \u091C\u093F\u0938\u092E\u0947\u0902 \u0939\u092E PHP variables \u0915\u094B \u0938\
  \u0940\u0927\u0947 strings \u0915\u0947 \u0905\u0902\u0926\u0930 \u0921\u093E\u0932\
  \u0924\u0947 \u0939\u0948\u0902. \u0907\u0938\u0938\u0947 \u0915\u094B\u0921 \u092A\
  \u0922\u093C\u0928\u0947 \u0914\u0930 \u0932\u093F\u0916\u0928\u0947 \u092E\u0947\
  \u0902 \u0906\u0938\u093E\u0928\u0940 \u0939\u094B\u0924\u0940 \u0939\u0948 \u0915\
  \u094D\u092F\u094B\u0902\u0915\u093F \u092F\u0939 variable\u2026"
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0907\u0902\u091F\u0930\u092A\
  \u094B\u0932\u0947\u0936\u0928"
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

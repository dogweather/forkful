---
title:                "डीबग आउटपुट प्रिंट करना"
date:                  2024-01-20T17:52:31.810614-07:00
model:                 gpt-4-1106-preview
simple_title:         "डीबग आउटपुट प्रिंट करना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
Debug output मतलब कोड से त्वरित संदेश निकालना, जैसे वैरिएबल की वैल्यू या कोड की स्थिति। Programmers इसका इस्तेमाल करते हैं ताकि कोड कैसे काम कर रहा है ये समझ सकें और गलतियाँ तलाश सकें।

## How to: (कैसे करें:)
Gleam में debug output प्रिंट करना सीधा है. आइए देखें कैसे:

```gleam
import gleam/io

pub fn main() {
  let my_variable = 42
  io.debug("Debug message: my_variable's value is")
  io.debug(my_variable)
}
```

जब आप यह कोड चलाएँगे, आपको console में यह output दिखाई देगा:

```
Debug message: my_variable's value is
42
```

## Deep Dive (गहराई से जानकारी):
पहले, log messages बहुत सामान्य थे, लेकिन अब debug libraries का इस्तेमाल होता है जैसे `gleam/io`. इससे कोड साफ़ रहता है और messages को manage करना आसान होता है। Alternatives में structured logging या remote debugging जैसे tools शामिल हैं। जब आप `io.debug` का इस्तेमाल करते हैं, तो Gleam runtime वो messages आपके console या configured logging system में भेजता है।

## See Also (और जानकारी के लिए):
- Gleam official documentation: [Gleam language](https://gleam.run/documentation/)
- Rust-based logging: [env_logger crate](https://docs.rs/env_logger/)
- Structured logging in Elixir: [telemetry](https://hexdocs.pm/telemetry/)
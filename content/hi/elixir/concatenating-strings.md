---
title:    "Elixir: स्ट्रिंग्स को संयुक्त करना"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

हमारे पास बहुत सारे अलग-अलग प्रोग्रामिंग भाषाएं हैं जिनमें से एक है Elixir। यह इनवर्सन ऑफ कंपीटेन्स और फंक्शनल प्रोग्रामिंग के लिए एक लोकप्रिय भाषा है। इलिक्सिर का मतलब है "जीवात्मा" जो कि इस भाषा के काम करने का एक अद्भुतह तरीका है। एक क्षेत्र हाल जिस पे लोग काफी प्रश्न करते हैं है, कि इंगिती ऑपरेशन यानि फॉर्मेटिंग कैसे करें? तो हम आज इसके बारे में चर्चा करेंगे।

## क्यों

अगर आप एलिक्सिर में प्रोग्रामिंग करते हैं तो आपने शायद ही String तथा Concatenate के बारे में सुना होगा। String जब दो या अधिक शब्दों का समूह होता है तो उसे नई string के रूप में मिलाया जाता है इसे concatenate कहते हैं। इलिक्सिर में भी हम इसी process को follow करते हैं। जब हम विभिन्न strings को मिलाते हैं तो हम नयी string को बनाते हैं जिसमें से पुरानी strings को हटा दिया जाता है। यह एक आसान और उपयोगी प्रक्रिया है जो डेटा मैनिपुलेशन में काफी मददगार होती है।

## कैसे

अब हम एलिक्सिर में string concatentation कैसे कर सकते हैं, इसके बारे में बात करेंगे। यह काफी आसान है, हम इसे "<<" ऑपरेटर के माध्यम से कर सकते हैं। नीचे coding examples दिए गए हैं

```elixir
name = "John"
greeting = "Hello "

full_greeting = greeting << name

IO.puts full_greeting

```

ऊपर का कोड रन करने पर आपको output में "Hello John" मिलेगा। इसमें हमने दो string "Hello " और "John" को concat करके एक नयी string "Hello John" बनाई ह
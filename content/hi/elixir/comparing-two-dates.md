---
title:                "Elixir: दो तारीखों की तुलना करना"
simple_title:         "दो तारीखों की तुलना करना"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

##क्यों
दो तारीखों को तुलना करने में शामिल होने के लिए क्यों किसी को निर्देशित करें।

##कैसे करे
```Elixir
defcompare_dates(date1, date2) do
  date1 < date2 
end
```
उपरोक्त कोड ब्लॉक में हमने Elixir में दो तारीखों को तुलना करने का सरल एक्साम्पल देखा है। कोड पर, हम "compare_dates" फंक्शन का उपयोग करते हैं जो दो तारीखों को लेता है और उन्हें तुलना करता है। फंक्शन अगर पहली तारीख दूसरी से कम हो तो true वापस करता है। प्रकरण तुलना के मद्देनजर ध्यान दें।

आउटपुट:
```Elixir
true
```

##गहराई में खोज
दो तारीखों की तुलना और तारीख समझ में आने वाले कॉडिंग असामान्य उदाहरणों का अध्ययन करते हुए अधिक जान सकते हैं। इसके अलावा, यह आपको दो तारीखों को तुलना करने के लिए अन्य Elixir प्रोग्रामिंग कार्यों के लिए भी मूल्यबोध और समझ देने में मदद कर सकता है।

##देखें
इस आर्टिकल में हमने देखा कि दो तारीखों को Elixir में तुलना कैसे किया जाता है और उससे क्या सीखा जा सकता है। अधिक जानने के लिए निम्नलिखित लिंक्स का उपयोग करे:

- Elixir Docs on Date Comparison: https://hexdocs.pm/elixir/Date.html#compare/2
- A Comprehensive Guide to Elixir Dates: http://elixir.org.uk/tutorials/dates.html
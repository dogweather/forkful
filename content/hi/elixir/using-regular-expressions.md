---
title:                "Elixir: रेग्युलर एक्सप्रेशन्स का प्रयोग"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## क्यों

अगर आप एलिक्सिर प्रोग्रामिंग को सीखने या उसमें माहिर होने की कोशिश कर रहे हैं, तो आपको निश्चित रूप से रेगुलर एक्सप्रेशन से अवगत होना चाहिए। यह भाषा कोडिंग के लिए बहुत ही उपयोगी है और आपको एलिक्सिर में अधिक समझदार बनाता है।

## कैसे

एलिक्सिर में रेगुलर एक्सप्रेशन का उपयोग करने के लिए, आपको आरम्भ में Regex मॉड्यूल को मशीनों में निष्पादित करना होगा। फिर, Regex.fuse फंक्शन को उपयोग करके, आप एक नया Regex ऑब्जेक्ट बना सकते हैं। उसके बाद, आप अपने पैटर्न को match फंक्शन के माध्यम से पास करके उस पर काम कर सकते हैं। नीचे दिए गए उदाहरण में, हम एक डिफॉल्टर पासवर्ड पैटर्न को ढूंढ रहे हैं और उसे बदलने का उदाहरण देख रहे हैं।

```Elixir
def password_security(password) do
  regex = Regex.fuse(~r/^(?=.*\d)(?=.*[a-z])(?=.*[A-Z])(?=.*[^\w\s]).+$/, opts: ~r/s/)
  
  if Regex.match?(regex, password) do
    IO.puts "Your password is strong!"
  else
    IO.puts "Please choose a stronger password."
  end
end

password_security("password123") 
# Output: Please choose a stronger password.

password_security("P@ssw0rd") 
# Output: Your password is strong!
```
## डीप डाइव

रेगुलर एक्सप्रेशन एक बहुत ही प्रभावी टूल है जो आपको टेक्स्ट स्ट्रिंग का प्रोसेसिंग करने का एक सुगम और शक्तिशाली तरीका प्रदान करता है। इसका उपयोग आप सभी प्रकार के टेक्स्ट सेटिंग्स में कर सकते हैं, जैसे इमेल ठीक करना, नोट्स टैक्स्ट को फॉर्मेट करना या अन्य कोई भी ऐसा काम जो एक्स
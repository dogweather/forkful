---
title:                "Arduino: भविष्य या भूतकाल में एक तारीख गणना करना"
programming_language: "Arduino"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्यों

क्या आपको कभी किसी स्पेशल अवसर के लिए तारीख की गणना करने की ज़रूरत हुई है? आजकल, हमारे जीवन में इस्तेमाल किये जाने वाले इलेक्ट्रॉनिक उपकरण जैसे कि अर्डुइनो बोर्ड, तारीख की गणना को आसान और तेज़ बना देते हैं। इस ब्लॉग पोस्ट के माध्यम से हम आपको बताएंगे कि अर्डुइनो में तारीख की गणना कैसे की जाती है। 

## कैसे करे

```arduino
// आवेदन को इनपुट के रूप में तारीख दिया जाना 
int date = 29022020;

// पूर्व के तिथि की गणना
int day = date%100;
int month = date/100%100;
int year = date/10000;

// कल की तिथि की गणना
int next_day = day+1;
int next_month = month;
int next_year = year;

// तारीख परिवर्तन
if (next_day > 31) {
  next_day = 1;
  next_month++;
}

if (next_month > 12) {
  next_month = 1;
  next_year++;
}

// प्रिंट करें
Serial.print("कल की तारीख: ");
Serial.print(next_day);
Serial.print("/");
Serial.print(next_month);
Serial.print("/");
Serial.print(next_year);
``` 

यहा आप देख सकते हैं कि हमने आर्डुइनो कोड के माध्यम से आसानी से कल की तारीख की गणना कर ली है। हमने `date` को तारीख के रूप में दिया गया है और इसे प्रिंट कर दिया गया है। इसके बाद, हमने `next_day`, `next_month` और `next_year` का प्रिंट किया है जो की कल की तारीख है। 

अधिक जानकारी के लिए, आप नीचे दिए गए स्रोत लिंक देख सकते हैं। 

## गहराई में

तारीख की गणना प्रोग्रामिंग के लिए बहुत ही महत्वपूर्ण है। इस से हम स्पेशल अवसरों को नहीं भूलते हैं और समय की बचत कर सकते हैं। अर्डुइनो जैसे उ
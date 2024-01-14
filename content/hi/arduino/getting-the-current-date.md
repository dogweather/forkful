---
title:    "Arduino: वर्तमान तारीख प्राप्त करना"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

# क्यों

क्या आपको पता है कि आप अपने अर्डुइनो में समय और तारीख को कैसे प्राप्त कर सकते हैं? जानिए इस ब्लॉग पोस्ट में क्यों और कैसे करना होगा।

# कैसे करें

**स्टेप 1:** सप्ताह और दिन की जांच करें
```Arduino
int weekDay = weekday();
int day = day();
```

**स्टेप 2:** महीना और साल को जांचें
```Arduino
int month = month();
int year = year();
```

**स्टेप 3:** अब सभी जानकारी को मिलाकर प्रिंट करें
```Arduino
Serial.print("Today is ");
Serial.print(weekDay);
Serial.print(", ");
Serial.print(day);
Serial.print(" ");
Serial.print(month);
Serial.print(" ");
Serial.print(year);
```

**आउटपुट:**
> Today is Monday, 15 March 2021

# गहराई में जाएं

अर्डुइनो में समय और तारीख को प्राप्त करना काफी आसान है। यह [Time स्लाइप लाइब्रेरी](https://www.arduino.cc/reference/en/libraries/time/) का उपयोग करती है जो आपको हजारों वर्षों तक की तारीख प्रबंधन की सुविधा प्रदान करती है। इसके अलावा इस लाइब्रेरी में दिन, महीने, साल, घंटे, मिनट और सेकंड को पढ़ने और सेट करने के लिए अनेक सुविधाएं शामिल हैं। इससे आपको जरूरत के हिसाब से समय और तारीख को प्राप्त करने के लिए अपने कोड में कस्टमाइजेशन करने की सुविधा मिलती है।

# देखें भी

- [Time स्लाइप लाइब्रेरी डॉक्यूमेंटेशन](https://www.arduino.cc/reference/en/libraries/time/)
- [अर्डुइनो में समय मॉड्यूल कैसे उपयोग करें](https://randomnerdtutorials.com/using-real-time-clock-rtc-arduino/) 
- [अर्डुइनो और DS1307 रियल टाइम क्लॉक मॉड्यूल के साथ तारीख और समय प्राप्त करना](https://circuitdigest.com/microcontroller-projects/arduino-to-get-current-date-time-from-ds1307-rtc)
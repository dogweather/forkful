---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:46:26.583136-06:00
description: "\u0E01\u0E32\u0E23\u0E14\u0E32\u0E27\u0E19\u0E4C\u0E42\u0E2B\u0E25\u0E14\
  \u0E2B\u0E19\u0E49\u0E32\u0E40\u0E27\u0E47\u0E1A\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\
  \u0E07\u0E01\u0E32\u0E23\u0E14\u0E36\u0E07\u0E40\u0E19\u0E37\u0E49\u0E2D\u0E2B\u0E32\
  \u0E02\u0E2D\u0E07\u0E21\u0E31\u0E19\u0E21\u0E32, \u0E42\u0E14\u0E22\u0E1B\u0E01\
  \u0E15\u0E34\u0E08\u0E30\u0E40\u0E1B\u0E47\u0E19\u0E43\u0E19\u0E23\u0E39\u0E1B\u0E41\
  \u0E1A\u0E1A HTML, \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E14\u0E39\u0E2B\u0E23\u0E37\u0E2D\
  \u0E1B\u0E23\u0E30\u0E21\u0E27\u0E25\u0E1C\u0E25\u0E43\u0E19\u0E40\u0E04\u0E23\u0E37\
  \u0E48\u0E2D\u0E07\u0E02\u0E2D\u0E07\u0E1C\u0E39\u0E49\u0E43\u0E0A\u0E49\u0E40\u0E2D\
  \u0E07.\u2026"
lastmod: '2024-03-17T21:57:56.520377-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E14\u0E32\u0E27\u0E19\u0E4C\u0E42\u0E2B\u0E25\u0E14\
  \u0E2B\u0E19\u0E49\u0E32\u0E40\u0E27\u0E47\u0E1A\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\
  \u0E07\u0E01\u0E32\u0E23\u0E14\u0E36\u0E07\u0E40\u0E19\u0E37\u0E49\u0E2D\u0E2B\u0E32\
  \u0E02\u0E2D\u0E07\u0E21\u0E31\u0E19\u0E21\u0E32, \u0E42\u0E14\u0E22\u0E1B\u0E01\
  \u0E15\u0E34\u0E08\u0E30\u0E40\u0E1B\u0E47\u0E19\u0E43\u0E19\u0E23\u0E39\u0E1B\u0E41\
  \u0E1A\u0E1A HTML, \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E14\u0E39\u0E2B\u0E23\u0E37\u0E2D\
  \u0E1B\u0E23\u0E30\u0E21\u0E27\u0E25\u0E1C\u0E25\u0E43\u0E19\u0E40\u0E04\u0E23\u0E37\
  \u0E48\u0E2D\u0E07\u0E02\u0E2D\u0E07\u0E1C\u0E39\u0E49\u0E43\u0E0A\u0E49\u0E40\u0E2D\
  \u0E07.\u2026"
title: "\u0E01\u0E32\u0E23\u0E14\u0E32\u0E27\u0E19\u0E4C\u0E42\u0E2B\u0E25\u0E14\u0E2B\
  \u0E19\u0E49\u0E32\u0E40\u0E27\u0E47\u0E1A"
weight: 42
---

## อะไร & ทำไม?
การดาวน์โหลดหน้าเว็บหมายถึงการดึงเนื้อหาของมันมา, โดยปกติจะเป็นในรูปแบบ HTML, เพื่อดูหรือประมวลผลในเครื่องของผู้ใช้เอง. โปรแกรมเมอร์ดาวน์โหลดหน้าเว็บเพื่อเก็บข้อมูล, ตรวจสอบการเปลี่ยนแปลง, หรือผสานรวมกับบริการเว็บ.

## วิธีการ:
ในเวอร์ชั่น C++ ปัจจุบัน, คุณสามารถใช้ `CURL` ไลบรารีเพื่อดาวน์โหลดเนื้อหาเว็บ. นี่คือตัวอย่างพื้นฐาน:

```cpp
#include <curl/curl.h>
#include <iostream>
#include <string>

static size_t writeCallback(void* contents, size_t size, size_t nmemb, void* userp){
    ((std::string*)userp)->append((char*)contents, size * nmemb);
    return size * nmemb;
}

int main() {
    CURL* curl;
    CURLcode res;
    std::string readBuffer;

    curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, writeCallback);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &readBuffer);
        res = curl_easy_perform(curl);
        curl_easy_cleanup(curl);

        if(res == CURLE_OK) {
            std::cout << readBuffer << std::endl;
        }
        else {
            std::cerr << "CURL ผิดพลาด: " << curl_easy_strerror(res) << std::endl;
        }
    }

    return 0;
}
```

ตัวอย่างผลลัพธ์:

```html
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
    ...
</head>
<body>
    <div>
        <h1>Example Domain</h1>
        <p>โดเมนนี้ใช้สำหรับการใช้เป็นตัวอย่างในเอกสาร. คุณอาจใช้โดเมนนี้ ...</p>
    </div>
</body>
</html>
```

## ศึกษาเพิ่มเติม
ในต้น, ไม่มีวิธีมาตรฐานในการดาวน์โหลดหน้าเว็บด้วย C++ เพียงอย่างเดียว. โปรแกรมเมอร์ใช้โซลูชันที่เฉพาะเจาะจงตามแพลตฟอร์มหรือไลบรารีของบุคคลที่สามที่หลากหลาย. ตอนนี้, `libcurl` เป็นไลบรารีที่ได้รับการสนับสนุนอย่างกว้างขวางและมีความยืดหยุ่นสูงสำหรับการถ่ายโอนข้อมูลด้วย URLs. ที่ถูกคอมไพล์และเชื่อมโยงกับโค้ด C++ ของคุณ, curl เป็นเครื่องมือที่ไปได้สุด.

ทางเลือกอื่นๆ ของ libcurl รวมถึง HTTPClientSession ของ Poco และ C++ Rest SDK (หรือที่รู้จักว่า Casablanca). ในขณะที่ libcurl เป็นที่พัฒนาบนฐานของ C และประมาณเท่าที่คุณสามารถไปได้ในระดับต่ำสุดเกี่ยวกับคำขอ HTTP, Poco และ Casablanca เสนออินเทอร์เฟซ C++ ที่เข้าใจง่ายกว่าซึ่งบางคนอาจชอบใช้.

ข้างใต้ฝา, เมื่อคุณดาวน์โหลดหน้าเว็บ, โปรโตคอล HTTP จะถูกกระตุ้น. คำขอ GET จะถูกส่งไปยังเซิร์ฟเวอร์, และถ้าทุกอย่างโอเค, เซิร์ฟเวอร์จะตอบกลับด้วยเนื้อหาที่ห่อหุ้มในคำตอบ HTTP.

## ดูเพิ่มเติม
- [เว็บไซต์อย่างเป็นทางการของ libcurl](https://curl.se/libcurl/)
- [GitHub Repo ของ C++ Rest SDK](https://github.com/microsoft/cpprestsdk)
- [โปรเจค Poco](https://pocoproject.org/)
- [HTTP บน Wikipedia](https://en.wikipedia.org/wiki/Hypertext_Transfer_Protocol)

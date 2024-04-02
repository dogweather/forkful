---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:07.594494-06:00
description: "\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E1E\
  \u0E23\u0E49\u0E2D\u0E21\u0E01\u0E32\u0E23\u0E23\u0E31\u0E1A\u0E23\u0E2D\u0E07\u0E04\
  \u0E27\u0E32\u0E21\u0E16\u0E39\u0E01\u0E15\u0E49\u0E2D\u0E07\u0E41\u0E1A\u0E1A\u0E1E\
  \u0E37\u0E49\u0E19\u0E10\u0E32\u0E19\u0E08\u0E30\u0E40\u0E01\u0E35\u0E48\u0E22\u0E27\
  \u0E02\u0E49\u0E2D\u0E07\u0E01\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E41\u0E19\u0E1A\u0E0A\
  \u0E37\u0E48\u0E2D\u0E1C\u0E39\u0E49\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u0E41\u0E25\
  \u0E30\u0E23\u0E2B\u0E31\u0E2A\u0E1C\u0E48\u0E32\u0E19\u0E44\u0E1B\u0E01\u0E31\u0E1A\
  \u0E04\u0E33\u0E02\u0E2D\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E04\u0E27\u0E1A\u0E04\u0E38\
  \u0E21\u0E01\u0E32\u0E23\u0E40\u0E02\u0E49\u0E32\u0E16\u0E36\u0E07\u2026"
lastmod: '2024-03-17T21:57:56.521312-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E1E\
  \u0E23\u0E49\u0E2D\u0E21\u0E01\u0E32\u0E23\u0E23\u0E31\u0E1A\u0E23\u0E2D\u0E07\u0E04\
  \u0E27\u0E32\u0E21\u0E16\u0E39\u0E01\u0E15\u0E49\u0E2D\u0E07\u0E41\u0E1A\u0E1A\u0E1E\
  \u0E37\u0E49\u0E19\u0E10\u0E32\u0E19\u0E08\u0E30\u0E40\u0E01\u0E35\u0E48\u0E22\u0E27\
  \u0E02\u0E49\u0E2D\u0E07\u0E01\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E41\u0E19\u0E1A\u0E0A\
  \u0E37\u0E48\u0E2D\u0E1C\u0E39\u0E49\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u0E41\u0E25\
  \u0E30\u0E23\u0E2B\u0E31\u0E2A\u0E1C\u0E48\u0E32\u0E19\u0E44\u0E1B\u0E01\u0E31\u0E1A\
  \u0E04\u0E33\u0E02\u0E2D\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E04\u0E27\u0E1A\u0E04\u0E38\
  \u0E21\u0E01\u0E32\u0E23\u0E40\u0E02\u0E49\u0E32\u0E16\u0E36\u0E07\u2026"
title: "\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E14\u0E49\
  \u0E27\u0E22\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E2A\u0E34\
  \u0E17\u0E18\u0E34\u0E4C\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19"
weight: 45
---

## อะไรและทำไม?

การส่งคำขอ HTTP พร้อมการรับรองความถูกต้องแบบพื้นฐานจะเกี่ยวข้องกับการแนบชื่อผู้ใช้งานและรหัสผ่านไปกับคำขอเพื่อควบคุมการเข้าถึง โปรแกรมเมอร์ทำเช่นนี้สำหรับโครงสร้างรับรองความถูกต้องอย่างง่ายเพื่อป้องกันทรัพยากรบนเซิร์ฟเวอร์

## วิธีการ:

นี่คือตัวอย่างพื้นฐานที่ใช้ไลบรารี `CURL` ใน C++ ก่อนที่จะเริ่มต้น ตรวจสอบให้แน่ใจว่าคุณได้ติดตั้ง `libcurl` แล้ว

```C++
#include <iostream>
#include <curl/curl.h>

// ฟังก์ชันย้อนกลับง่ายๆ เพื่อจัดการข้อมูลที่ได้รับจาก curl
static size_t WriteCallback(void *contents, size_t size, size_t nmemb, void *userp) {
    ((std::string*)userp)->append((char*)contents, size * nmemb);
    return size * nmemb;
}

int main() {
    CURL *curl;
    CURLcode res;
    std::string readBuffer;

    curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://yourapi.com/data");
        curl_easy_setopt(curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
        curl_easy_setopt(curl, CURLOPT_USERNAME, "user");
        curl_easy_setopt(curl, CURLOPT_PASSWORD, "pass");
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &readBuffer);
        
        // ดำเนินการคำขอ และตรวจสอบข้อผิดพลาด
        res = curl_easy_perform(curl);
        if(res != CURLE_OK) {
            fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));
        } else {
            std::cout << readBuffer << std::endl;
        }
        
        // การทำความสะอาด
        curl_easy_cleanup(curl);
    }
    return 0;
}
```

คุณจะเห็นการตอบกลับจากเซิร์ฟเวอร์ถูกพิมพ์ออกที่คอนโซล โดยสมมติว่าไม่มีข้อผิดพลาดใดๆ เกิดขึ้น

## พิจารณาลึกซึ้ง

การรับรองความถูกต้องแบบพื้นฐานเป็นวิธีที่เก่าแก่ ย้อนกลับไปในช่วงต้นๆ ของ HTTP ปัจจุบัน ความชอบของอุตสาหกรรมมีแนวโน้มเอนไปทางวิธีการที่ปลอดภัยกว่า เช่น OAuth และโทเค็น แม้ว่าการรับรองความถูกต้องแบบพื้นฐานจะยังคงใช้งานอยู่ บ่อยครั้งสำหรับระบบภายในหรือระบบง่ายๆ ที่ชั้นความปลอดภัยที่หนักหน่วงเป็นเรื่องที่เกินไป

ภายใน ชื่อผู้ใช้และรหัสผ่านของคุณจะถูกเข้ารหัสแบบ base64 และซ่อนอยู่ในส่วนหัว HTTP มันเป็นวิธีง่ายๆ แต่ไม่ปลอดภัยหากไม่ผ่าน HTTPS เพราะ base64 สามารถถอดรหัสได้ง่าย—ทำให้ HTTPS เป็นสิ่งจำเป็น

หาก `libcurl` ไม่ตรงกับความชอบของคุณ พิจารณาทางเลือกอื่นเช่นไลบรารี `cpp-httplib` หรือคุณสามารถใช้ `Boost.Beast` เพื่อการเข้ามือทำมากขึ้น

## ดูเพิ่มเติม

- [libcurl](https://curl.se/libcurl/)
- [พื้นที่เก็บข้อมูล GitHub ของ cpp-httplib](https://github.com/yhirose/cpp-httplib)
- [เอกสาร Boost.Beast](https://www.boost.org/doc/libs/master/libs/beast/doc/html/index.html)

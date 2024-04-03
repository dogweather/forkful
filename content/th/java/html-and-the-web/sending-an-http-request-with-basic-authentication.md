---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:21.152683-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: Java \u0E17\u0E33\u0E43\u0E2B\
  \u0E49\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E1E\u0E23\
  \u0E49\u0E2D\u0E21\u0E01\u0E32\u0E23\u0E23\u0E31\u0E1A\u0E23\u0E2D\u0E07\u0E04\u0E27\
  \u0E32\u0E21\u0E16\u0E39\u0E01\u0E15\u0E49\u0E2D\u0E07\u0E41\u0E1A\u0E1A\u0E1E\u0E37\
  \u0E49\u0E19\u0E10\u0E32\u0E19\u0E40\u0E1B\u0E47\u0E19\u0E40\u0E23\u0E37\u0E48\u0E2D\
  \u0E07\u0E07\u0E48\u0E32\u0E22\u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49\u0E04\u0E25\u0E32\
  \u0E2A `HttpURLConnection` \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E15\u0E31\u0E27\
  \u0E2D\u0E22\u0E48\u0E32\u0E07\u0E17\u0E35\u0E48\u0E23\u0E27\u0E14\u0E40\u0E23\u0E47\
  \u0E27."
lastmod: '2024-03-17T21:57:56.081004-06:00'
model: gpt-4-0125-preview
summary: "Java \u0E17\u0E33\u0E43\u0E2B\u0E49\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\
  \u0E33\u0E02\u0E2D HTTP \u0E1E\u0E23\u0E49\u0E2D\u0E21\u0E01\u0E32\u0E23\u0E23\u0E31\
  \u0E1A\u0E23\u0E2D\u0E07\u0E04\u0E27\u0E32\u0E21\u0E16\u0E39\u0E01\u0E15\u0E49\u0E2D\
  \u0E07\u0E41\u0E1A\u0E1A\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19\u0E40\u0E1B\u0E47\
  \u0E19\u0E40\u0E23\u0E37\u0E48\u0E2D\u0E07\u0E07\u0E48\u0E32\u0E22\u0E42\u0E14\u0E22\
  \u0E43\u0E0A\u0E49\u0E04\u0E25\u0E32\u0E2A `HttpURLConnection` \u0E19\u0E35\u0E48\
  \u0E04\u0E37\u0E2D\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E17\u0E35\u0E48\
  \u0E23\u0E27\u0E14\u0E40\u0E23\u0E47\u0E27."
title: "\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E14\u0E49\
  \u0E27\u0E22\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E2A\u0E34\
  \u0E17\u0E18\u0E34\u0E4C\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19"
weight: 45
---

## วิธีการ:
Java ทำให้การส่งคำขอ HTTP พร้อมการรับรองความถูกต้องแบบพื้นฐานเป็นเรื่องง่ายโดยใช้คลาส `HttpURLConnection` นี่คือตัวอย่างที่รวดเร็ว:

```java
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.Base64;

public class BasicAuthRequest {

    public static void main(String[] args) {
        try {
            URL url = new URL("http://example.com/resource");
            HttpURLConnection connection = (HttpURLConnection) url.openConnection();
            
            String userCredentials = "user:password";
            String basicAuth = "Basic " + new String(Base64.getEncoder().encode(userCredentials.getBytes(StandardCharsets.UTF_8)));
            connection.setRequestProperty("Authorization", basicAuth);

            int responseCode = connection.getResponseCode();
            System.out.println("รหัสตอบสนอง: " + responseCode);

            if (responseCode == HttpURLConnection.HTTP_OK) {
                BufferedReader in = new BufferedReader(new InputStreamReader(connection.getInputStream()));
                String inputLine;
                StringBuilder response = new StringBuilder();

                while ((inputLine = in.readLine()) != null) {
                    response.append(inputLine);
                }
                in.close();

                System.out.println(response.toString());
            } else {
                System.out.println("คำขอ GET ไม่ได้ผล");
            }

        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```
ตัวอย่างผลลัพธ์:
```
รหัสตอบสนอง: 200
{ "message": "นี่คือคำตอบจากทรัพยากรที่ถูกป้องกัน!" }
```

## ค้นดูลึกลงไปกว่านั้น
การรับรองความถูกต้องแบบพื้นฐานมีมาตั้งแต่ช่วงเริ่มต้นของ HTTP มันทำงานโดยการส่งข้อมูลรับรองความถูกต้องที่ถูกเข้ารหัส base64 ในหัวข้อ ทำให้มันง่ายแต่ไม่ค่อยปลอดภัยหากไม่ใช้ HTTPS เนื่องจากข้อมูลรับรองความถูกต้องสามารถถูกถอดรหัสได้อย่างง่ายดาย

ทางเลือกเช่น OAuth เพิ่มอีกชั้นของความปลอดภัยโดยใช้โทเค็นแทน การรับรองความถูกต้องแบบโทเค็นได้รับการชื่นชอบมากขึ้นในปัจจุบันโดยเฉพาะสำหรับ API แบบ RESTful

เมื่อทำการประยุกต์ใช้การรับรองความถูกต้องแบบพื้นฐานใน Java วิธีที่แนะนำตั้งแต่ Java 11 คือใช้คลาสใหม่ `HttpClient` มันมีความหลากหลายมากขึ้นและรองรับ HTTP/2 ตั้งแต่แรก อย่างไรก็ตามสำหรับความต้องการพื้นฐานหรือระบบเก่า `HttpURLConnection` ยังคงเป็นตัวเลือกที่เป็นไปได้

## ดูเพิ่มเติมที่
- [RFC 7617 - มาตรฐาน 'Basic' ของวิธีการรับรองความถูกต้อง HTTP](https://tools.ietf.org/html/rfc7617)
- [เอกสาร API ลูกค้า HTTP ของ Oracle Java 11](https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpClient.html)
- [คู่มือ Baeldung ในการส่งคำขอ HTTP ด้วย Java](https://www.baeldung.com/java-http-request)

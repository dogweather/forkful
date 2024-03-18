---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:13.962316-06:00
description: "\u0E40\u0E23\u0E32\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E1E\
  \u0E23\u0E49\u0E2D\u0E21\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\
  \u0E2A\u0E34\u0E17\u0E18\u0E34\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19\u0E40\u0E1E\
  \u0E37\u0E48\u0E2D\u0E40\u0E02\u0E49\u0E32\u0E16\u0E36\u0E07\u0E17\u0E23\u0E31\u0E1E\
  \u0E22\u0E32\u0E01\u0E23\u0E17\u0E35\u0E48\u0E16\u0E39\u0E01\u0E1B\u0E49\u0E2D\u0E07\
  \u0E01\u0E31\u0E19 \u0E42\u0E14\u0E22\u0E01\u0E32\u0E23\u0E23\u0E27\u0E21\u0E02\u0E49\
  \u0E2D\u0E21\u0E39\u0E25\u0E23\u0E31\u0E1A\u0E23\u0E2D\u0E07\u0E02\u0E2D\u0E07\u0E1C\
  \u0E39\u0E49\u0E43\u0E0A\u0E49\u0E43\u0E19\u0E2A\u0E48\u0E27\u0E19\u0E2B\u0E31\u0E27\
  \u0E02\u0E2D\u0E07\u0E04\u0E33\u0E02\u0E2D\u2026"
lastmod: '2024-03-17T21:57:56.226072-06:00'
model: gpt-4-0125-preview
summary: "\u0E40\u0E23\u0E32\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E1E\
  \u0E23\u0E49\u0E2D\u0E21\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\
  \u0E2A\u0E34\u0E17\u0E18\u0E34\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19\u0E40\u0E1E\
  \u0E37\u0E48\u0E2D\u0E40\u0E02\u0E49\u0E32\u0E16\u0E36\u0E07\u0E17\u0E23\u0E31\u0E1E\
  \u0E22\u0E32\u0E01\u0E23\u0E17\u0E35\u0E48\u0E16\u0E39\u0E01\u0E1B\u0E49\u0E2D\u0E07\
  \u0E01\u0E31\u0E19 \u0E42\u0E14\u0E22\u0E01\u0E32\u0E23\u0E23\u0E27\u0E21\u0E02\u0E49\
  \u0E2D\u0E21\u0E39\u0E25\u0E23\u0E31\u0E1A\u0E23\u0E2D\u0E07\u0E02\u0E2D\u0E07\u0E1C\
  \u0E39\u0E49\u0E43\u0E0A\u0E49\u0E43\u0E19\u0E2A\u0E48\u0E27\u0E19\u0E2B\u0E31\u0E27\
  \u0E02\u0E2D\u0E07\u0E04\u0E33\u0E02\u0E2D\u2026"
title: "\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E14\u0E49\
  \u0E27\u0E22\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E2A\u0E34\
  \u0E17\u0E18\u0E34\u0E4C\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19"
---

{{< edit_this_page >}}

## อะไร & ทำไม?
เราส่งคำขอ HTTP พร้อมการตรวจสอบสิทธิพื้นฐานเพื่อเข้าถึงทรัพยากรที่ถูกป้องกัน โดยการรวมข้อมูลรับรองของผู้ใช้ในส่วนหัวของคำขอ โปรแกรมเมอร์ใช้สิ่งนี้สำหรับระบบ auth ที่ง่าย โดยส่วนใหญ่เหมาะกับวิธีการที่รวดเร็วและง่ายดาย

## วิธีการ:
เรามาเริ่มสำรวจโค้ดกันเลย ด้านล่างนี้คือตัวอย่างขั้นต่ำโดยใช้ C# ในการส่งคำขอ HTTP พร้อมการตรวจสอบสิทธิพื้นฐาน:

```C#
using System;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Text;
using System.Threading.Tasks;

class Program
{
    static async Task Main()
    {
        using (var client = new HttpClient())
        {
            var credentials = Convert.ToBase64String(Encoding.ASCII.GetBytes("username:password"));
            client.DefaultRequestHeaders.Authorization = new AuthenticationHeaderValue("Basic", credentials);

            HttpResponseMessage response = await client.GetAsync("http://yourapi.com/protected");

            if (response.IsSuccessStatusCode)
            {
                string responseBody = await response.Content.ReadAsStringAsync();
                Console.WriteLine(responseBody);
            }
            else
            {
                Console.WriteLine($"Error: {response.StatusCode}");
            }
        }
    }
}
```
เรียกใช้งานนี้ และหาก endpoint และ creds ของคุณถูกต้อง คุณจะได้รับทรัพยากร ถ้าไม่ คุณจะเห็นรหัสสถานะข้อผิดพลาด

## ลงลึก
การตรวจสอบสิทธิพื้นฐานนั้นเก่ามาก ย้อนหลังไปถึงยุคแรกๆ ของอินเทอร์เน็ต มันง่ายมาก: แค่เข้ารหัส base64 "username:password" และแนบมันไปกับส่วนหัว `Authorization`

มีทางเลือกที่มีความปลอดภัยมากขึ้น: OAuth2, คีย์ API หรือโทเค็น JWT การตรวจสอบสิทธิพื้นฐานยังคงมีอยู่เนื่องจากความเรียบง่ายของมัน แต่ต้องระวังว่ามันไม่ได้ถูกเข้ารหัส และสามารถถูกดักฟังได้หากไม่ได้ใช้ผ่าน HTTPS

เมื่อคุณใช้วิธีนี้ โปรดจำไว้ว่า:
- ใช้ HTTPS เสมอเพื่อป้องกันข้อมูลรับรองในระหว่างการถ่ายโอน
- มันเหมือนกับการทิ้งกุญแจบ้านไว้ใต้เสื่อปูพื้น – สะดวกแต่เปราะบาง ดังนั้นใช้สำหรับสถานการณ์ที่มีความเสี่ยงต่ำ
- เนื่องจากข้อมูลรับรองถูกส่งต่อไปกับทุกคำขอ มันจึงไม่ใช่วิธีที่มีประสิทธิภาพสูงสุดสำหรับระบบที่มีภาระงานมาก

## ดูเพิ่มเติม
- [เอกสารประกอบคลาส HttpClient ของ Microsoft](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)
- [คำอธิบายการตรวจสอบสิทธิพื้นฐานของ Mozilla](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [โกงใบสรุปการตรวจสอบสิทธิของ OWASP](https://owasp.org/www-project-cheat-sheets/cheatsheets/Authentication_Cheat_Sheet.html)

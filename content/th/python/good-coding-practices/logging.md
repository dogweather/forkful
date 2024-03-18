---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:48:46.984514-06:00
description: "\u0E01\u0E32\u0E23\u0E1A\u0E31\u0E19\u0E17\u0E36\u0E01\u0E01\u0E32\u0E23\
  \u0E40\u0E2B\u0E15\u0E38\u0E01\u0E32\u0E23\u0E13\u0E4C (Logging) \u0E40\u0E1B\u0E47\
  \u0E19\u0E01\u0E23\u0E30\u0E1A\u0E27\u0E19\u0E01\u0E32\u0E23\u0E02\u0E2D\u0E07\u0E01\
  \u0E32\u0E23\u0E1A\u0E31\u0E19\u0E17\u0E36\u0E01\u0E23\u0E32\u0E22\u0E25\u0E30\u0E40\
  \u0E2D\u0E35\u0E22\u0E14\u0E40\u0E2B\u0E15\u0E38\u0E01\u0E32\u0E23\u0E13\u0E4C\u0E15\
  \u0E48\u0E32\u0E07\u0E46 \u0E02\u0E13\u0E30\u0E17\u0E35\u0E48\u0E42\u0E1B\u0E23\u0E41\
  \u0E01\u0E23\u0E21\u0E01\u0E33\u0E25\u0E31\u0E07\u0E17\u0E33\u0E07\u0E32\u0E19\u2026"
lastmod: '2024-03-17T21:57:55.768689-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E1A\u0E31\u0E19\u0E17\u0E36\u0E01\u0E01\u0E32\u0E23\
  \u0E40\u0E2B\u0E15\u0E38\u0E01\u0E32\u0E23\u0E13\u0E4C (Logging) \u0E40\u0E1B\u0E47\
  \u0E19\u0E01\u0E23\u0E30\u0E1A\u0E27\u0E19\u0E01\u0E32\u0E23\u0E02\u0E2D\u0E07\u0E01\
  \u0E32\u0E23\u0E1A\u0E31\u0E19\u0E17\u0E36\u0E01\u0E23\u0E32\u0E22\u0E25\u0E30\u0E40\
  \u0E2D\u0E35\u0E22\u0E14\u0E40\u0E2B\u0E15\u0E38\u0E01\u0E32\u0E23\u0E13\u0E4C\u0E15\
  \u0E48\u0E32\u0E07\u0E46 \u0E02\u0E13\u0E30\u0E17\u0E35\u0E48\u0E42\u0E1B\u0E23\u0E41\
  \u0E01\u0E23\u0E21\u0E01\u0E33\u0E25\u0E31\u0E07\u0E17\u0E33\u0E07\u0E32\u0E19\u2026"
title: "\u0E01\u0E32\u0E23\u0E1A\u0E31\u0E19\u0E17\u0E36\u0E01\u0E25\u0E47\u0E2D\u0E01"
---

{{< edit_this_page >}}

## การบันทึกการเหตุการณ์กับ Python: มันคืออะไรและทำไม?

การบันทึกการเหตุการณ์ (Logging) เป็นกระบวนการของการบันทึกรายละเอียดเหตุการณ์ต่างๆ ขณะที่โปรแกรมกำลังทำงาน มีจุดประสงค์เพื่อให้สามารถวิเคราะห์ย้อนหลังและติดตามสถานการณ์ได้ในเวลาจริง นักพัฒนาทำการบันทึกการเหตุการณ์เพราะว่ามันช่วยในการตรวจสอบปัญหา, ติดตามประสิทธิภาพ และติดตามการกระทำของผู้ใช้เพื่อความปลอดภัยและการวิเคราะห์

## วิธีการ:

Python มีโมดูลมาตรฐานสำหรับการบันทึกการเหตุการณ์ นี่คือการตั้งค่าพื้นฐาน:

```Python
import logging

# การคอนฟิกพื้นฐานของการบันทึก
logging.basicConfig(level=logging.INFO)

# ข้อความการบันทึก
logging.debug('ข้อความสำหรับการแก้ไขบั๊ก')
logging.info('ข้อมูลเกี่ยวกับสิ่งที่โปรแกรมของคุณเพิ่งทำ')
logging.warning('ข้อความเตือน')
logging.error('เกิดข้อผิดพลาด')
logging.critical('โปรแกรมไม่สามารถกู้คืนได้!')
```
เมื่อคุณเรียกใช้โค้ดนี้ คุณจะเห็นผลลัพธ์ต่อไปนี้ (เนื่องจากระดับเริ่มต้นคือ WARNING ดังนั้นข้อความ debug และ info จะไม่ถูกแสดง):
```
WARNING:root:ข้อความเตือน
ERROR:root:เกิดข้อผิดพลาด
CRITICAL:root:โปรแกรมไม่สามารถกู้คืนได้!
```
คุณสามารถตั้งค่าการบันทึกไปยังไฟล์แทนที่จะเป็นคอนโซลได้:
```Python
logging.basicConfig(filename='app.log', filemode='w', level=logging.INFO)
```
ตอนนี้การบันทึกของคุณจะถูกนำไปยังไฟล์ 'app.log'

## ศึกษาลึก

การบันทึกการเหตุการณ์ได้มีมาตั้งแต่ยุคแรกของการพัฒนาซอฟต์แวร์ โดยที่บันทึกระบบเป็นหนึ่งในรูปแบบของการจัดเก็บข้อมูลถาวรที่เก่าแก่ที่สุดนอกเหนือจากไฟล์ที่ถือข้อมูลจริง แม้ประวัติศาสตร์จะผ่านไป แต่แนวคิดหลักของการบันทึกการเหตุการณ์ยังคงไม่เปลี่ยนแปลง แม้ว่าเครื่องมือต่างๆ อาจพัฒนาไป

โมดูล `logging` ของ Python มีความสามารถและความยืดหยุ่นสูง มันช่วยให้โปรแกรมเมอร์สามารถตั้งค่าระดับการบันทึก (DEBUG, INFO, WARNING, ERROR, CRITICAL) ที่สามารถช่วยในการจำแนกและกรองการบันทึก เป็นระบบต้นไม้ที่ทำให้สามารถมีความสัมพันธ์แบบพ่อ-ลูกระหว่างตัวบันทึกการเหตุการณ์และส่งต่อข้อความตามลำดับได้

ทางเลือกอื่น ๆ รวมถึงไลบรารีของบุคคลที่สามเช่น Loguru หรือ structlog ซึ่งให้คุณสมบัติเพิ่มเติมและหน้าตาที่ง่ายกว่าโมดูลบันทึกมาตรฐาน พวกเขาสามารถให้ผลลัพธ์ที่สวยงามยิ่งขึ้น การซีเรียลไลซ์ข้อมูลที่มีโครงสร้างที่ดีขึ้น และวิธีที่ง่ายกว่าในการจัดการกับการตั้งค่าบันทึก

ในแง่ของการนำไปใช้งาน การตั้งค่าการบันทึกเป็นสิ่งสำคัญที่ต้องทำตั้งแต่เริ่มต้นของการใช้งาน การกำหนดค่าในระดับโมดูลโดยใช้ `logging.getLogger(__name__)` เป็นวิธีตามแนวทางที่ดีที่สุดของ Python ด้านการบันทึก

โดยทั่วไป การบันทึกไม่ควรส่งผลกระทบต่อปร Performance ของแอพพลิเคชันอย่างมาก อย่างไรก็ตาม ควรให้ความระมัดระวังกับสิ่งที่บันทึก: การบันทึกที่มีรายละเอียดมากเกินไป โดยเฉพาะอย่างยิ่งในระดับ DEBUG อาจทำให้แอพพลิเคชันช้าลงและรวดเร็วในการเต็มพื้นที่จัดเก็บข้อมูลบันทึก

## ดูเพิ่มเติม
สำหรับข้อมูลเพิ่มเติมเกี่ยวกับโมดูลการบันทึกของ Python ให้ตรวจสอบหนังสือคู่มือการบันทึกของ Python ที่เป็นทางการสำหรับตัวอย่างที่ยอดเยี่ยมและแนวทางปฏิบัติที่ดี: https://docs.python.org/3/howto/logging-cookbook.html

สำหรับการศึกษาเจาะลึกเกี่ยวกับการบันทึกที่มีโครงสร้างและวิธีที่มันสามารถช่วยให้การบันทึกข้อมูลมีข้อมูลเพิ่มเติมและง่ายต่อการวิเคราะห์, Loguru มีเอกสารช่วยอธิบายได้ดี: https://loguru.readthedocs.io 

นอกจากนี้ พิจารณาการดูที่วิธีการแอพพลิเคชัน 12 ปัจจัย เฉพาะในส่วนของบันทึกการเหตุการณ์สำหรับมุมมองสมัยใหม่เกี่ยวกับการบันทึกแอพพลิเคชัน: https://12factor.net/logs
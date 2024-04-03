---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:50:31.849220-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E19\u0E35\u0E48\u0E04\
  \u0E37\u0E2D\u0E04\u0E39\u0E48\u0E21\u0E37\u0E2D\u0E44\u0E23\u0E49\u0E21\u0E25\u0E17\
  \u0E34\u0E19\u0E43\u0E19\u0E01\u0E32\u0E23\u0E15\u0E31\u0E14\u0E2D\u0E31\u0E01\u0E02\
  \u0E23\u0E30\u0E04\u0E33\u0E1E\u0E39\u0E14\u0E17\u0E35\u0E48\u0E19\u0E48\u0E32\u0E23\
  \u0E33\u0E04\u0E32\u0E0D\u0E2D\u0E2D\u0E01\u0E08\u0E32\u0E01\u0E2A\u0E15\u0E23\u0E34\
  \u0E07\u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13\u0E43\u0E19 TypeScript."
lastmod: '2024-03-17T21:57:55.931474-06:00'
model: gpt-4-0125-preview
summary: "\u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E04\u0E39\u0E48\u0E21\u0E37\u0E2D\
  \u0E44\u0E23\u0E49\u0E21\u0E25\u0E17\u0E34\u0E19\u0E43\u0E19\u0E01\u0E32\u0E23\u0E15\
  \u0E31\u0E14\u0E2D\u0E31\u0E01\u0E02\u0E23\u0E30\u0E04\u0E33\u0E1E\u0E39\u0E14\u0E17\
  \u0E35\u0E48\u0E19\u0E48\u0E32\u0E23\u0E33\u0E04\u0E32\u0E0D\u0E2D\u0E2D\u0E01\u0E08\
  \u0E32\u0E01\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13\u0E43\
  \u0E19 TypeScript."
title: "\u0E01\u0E32\u0E23\u0E25\u0E1A\u0E40\u0E04\u0E23\u0E37\u0E48\u0E2D\u0E07\u0E2B\
  \u0E21\u0E32\u0E22\u0E2D\u0E31\u0E0D\u0E1B\u0E23\u0E30\u0E01\u0E32\u0E28\u0E2D\u0E2D\
  \u0E01\u0E08\u0E32\u0E01\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 9
---

## วิธีการ:
นี่คือคู่มือไร้มลทินในการตัดอักขระคำพูดที่น่ารำคาญออกจากสตริงของคุณใน TypeScript

```typescript
// ตัวเลือก A: การเปลี่ยนแปลงอัญประกาศเดี่ยวหรือคู่โดยใช้ regex
function removeQuotes(input: string): string {
  return input.replace(/^["']|["']$/g, '');
}

console.log(removeQuotes(`"Quoted string"`)); // Quoted string
console.log(removeQuotes(`'Another one'`)); // Another one

// ตัวเลือก B: การจัดการกับสตริงที่เริ่มและจบด้วยอัญประกาศที่แตกต่างกัน
function removeMismatchedQuotes(input: string): string {
  return input.replace(/^(['"])(.*?)(?<!\1)\1$/, '$2');
}

console.log(removeMismatchedQuotes(`"Mismatched'`)); // "Mismatched'

// ตัวเลือก C: ตัดอัญประกาศหลากหลายประเภท
function removeAllQuotes(input: string): string {
  return input.replace(/['"]+/g, '');
}

console.log(removeAllQuotes(`"'Mix'n'Match'"`)); // Mix'n'Match
```

## ประดุจดำดิ่ง
ตั้งแต่ยังก่อนที่ TypeScript จะมีอยู่, โค้ดเดอร์ JavaScript ก็ต้องเผชิญกับความยุ่งยากเรื่องอัญประกาศอยู่แล้ว และเรื่องราวก็ไม่ต่างกันสำหรับ TypeScript ตามเวลาที่เปลี่ยนไป วิธีที่เราตัดสตริงก็เปลี่ยนไป ในปัจจุบัน ด้วยพลังของ regex, เราสามารถตัดสินใจไม่ใช้วิธีตัดสตริงที่ยุ่งยากหรือวิธีอื่นที่ต้องทำซ้ำๆ

แม้ว่าตัวอย่างด้านบนควรครอบคลุมความต้องการส่วนใหญ่ของคุณแล้ว จำไว้ว่า การใช้อัญประกาศสามารถทำให้ซับซ้อนได้ อัญประกาศที่ซ้อนกัน, ไม่ตรงกัน, และถูกหลบหนีเป็นคนเล่นตลกที่รอจะทำให้คุณพลาด สำหรับเหล่านี้ คุณอาจต้องการรูปแบบที่ซับซ้อนขึ้นหรือแม้กระทั่งตัวแยกวิเคราะห์เพื่อจัดการกับทุกกรณีที่ซับซ้อน

ทางเลือก? บางคนชอบไปกับไลบรารี่เช่น lodash ที่มีวิธีการเช่น `trim` และ `trimStart` / `trimEnd` ซึ่งสามารถปรับเปลี่ยนเพื่อตัดอัญประกาศได้หากคุณกำหนดอักขระที่คุณต้องการตัด

และสำหรับคุณที่ชื่นชอบ TypeScript, อย่าลืมเรื่องประเภทข้อมูล ในที่นี้เรากำลังจัดการกับสตริงมากที่สุด แต่เมื่อคุณกำลังทำงานกับข้อมูลอินพุตจากผู้ใช้หรือการแยกวิเคราะห์ การใช้ตัวป้องกันประเภทหรือแม้แต่ generics สามารถช่วยให้คุณรักษาโค้ดของคุณให้ปลอดภัยเหมือนกับอัญประกาศของคุณถูกตัดทิ้งไปแล้ว

## ดูเพิ่มเติม
ตรวจสอบสถานที่เสมือนเหล่านี้สำหรับข้อมูลเพิ่มเติม:

- MDN Web Docs บน regex (https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- TypeScript เอกสารอย่างเป็นทางการ (https://www.typescriptlang.org/docs/)
- You Don't Need Lodash/Underscore – String Helpers (https://github.com/you-dont-need/You-Dont-Need-Lodash-Underscore#strings)
- Stack Overflow: ลุยห้วงที่นักพัฒนานับไม่ถ้วนได้ต่อสู้กับความไม่ลงรอยกันของอัญประกาศ (https://stackoverflow.com/)

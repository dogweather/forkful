---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:18.735737-06:00
description: "Regular expressions (regex) \u0E43\u0E19 JavaScript \u0E40\u0E1B\u0E47\
  \u0E19\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E17\u0E35\u0E48\u0E43\u0E0A\u0E49\u0E2A\
  \u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E08\u0E31\u0E1A\u0E04\u0E39\u0E48\u0E01\u0E25\u0E38\
  \u0E48\u0E21\u0E2D\u0E31\u0E01\u0E02\u0E23\u0E30\u0E43\u0E19\u0E2A\u0E15\u0E23\u0E34\
  \u0E07 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E43\
  \u0E0A\u0E49\u0E1E\u0E27\u0E01\u0E40\u0E02\u0E32\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\
  \u0E01\u0E32\u0E23\u0E04\u0E49\u0E19\u0E2B\u0E32, \u0E2A\u0E01\u0E31\u0E14, \u0E41\
  \u0E25\u0E30\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\
  \u0E21,\u2026"
lastmod: '2024-03-17T21:57:56.592232-06:00'
model: gpt-4-0125-preview
summary: "Regular expressions (regex) \u0E43\u0E19 JavaScript \u0E40\u0E1B\u0E47\u0E19\
  \u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E17\u0E35\u0E48\u0E43\u0E0A\u0E49\u0E2A\u0E33\
  \u0E2B\u0E23\u0E31\u0E1A\u0E08\u0E31\u0E1A\u0E04\u0E39\u0E48\u0E01\u0E25\u0E38\u0E48\
  \u0E21\u0E2D\u0E31\u0E01\u0E02\u0E23\u0E30\u0E43\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07\
  \ \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E43\
  \u0E0A\u0E49\u0E1E\u0E27\u0E01\u0E40\u0E02\u0E32\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\
  \u0E01\u0E32\u0E23\u0E04\u0E49\u0E19\u0E2B\u0E32, \u0E2A\u0E01\u0E31\u0E14, \u0E41\
  \u0E25\u0E30\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\
  \u0E21, \u0E0B\u0E36\u0E48\u0E07\u0E17\u0E33\u0E43\u0E2B\u0E49\u0E2A\u0E32\u0E21\
  \u0E32\u0E23\u0E16\u0E14\u0E33\u0E40\u0E19\u0E34\u0E19\u0E01\u0E32\u0E23\u0E1B\u0E23\
  \u0E30\u0E21\u0E27\u0E25\u0E1C\u0E25\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E17\u0E35\u0E48\
  \u0E21\u0E35\u0E1E\u0E25\u0E31\u0E07\u0E14\u0E49\u0E27\u0E22\u0E42\u0E04\u0E49\u0E14\
  \u0E17\u0E35\u0E48\u0E01\u0E23\u0E30\u0E0A\u0E31\u0E1A."
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E19\u0E34\u0E1E\u0E08\u0E19\u0E4C\u0E1B\
  \u0E01\u0E15\u0E34"
weight: 11
---

## วิธีการ:


### การจับคู่พื้นฐาน
เพื่อเริ่มต้น, คุณสามารถสร้างรูปแบบ regex ง่ายๆ และใช้มันในการค้นหาคู่ในสตริง ที่นี่, เราจะค้นหาคำว่า "code":

```javascript
const str = "I love to code in JavaScript.";
const pattern = /code/;
const result = pattern.test(str);
console.log(result); // true
```

### การใช้ `String.prototype.match()`
เพื่อดึงอาร์เรย์ของคู่ที่ตรงกัน:

```javascript
const matches = str.match(/code/);
console.log(matches[0]); // "code"
console.log(matches.index); // 10
```

### การค้นหาทั่วโลก
เพื่อค้นหาคู่ทั้งหมด, ใช้ธง `g`:

```javascript
const globalMatches = str.match(/o/g);
console.log(globalMatches); // ["o", "o", "o"]
```

### การจับคู่โดยไม่คำนึงถึงตัวพิมพ์ใหญ่หรือเล็ก
ธง `i` จะไม่คำนึงถึงตัวพิมพ์ใหญ่หรือเล็ก:

```javascript
const caseInsensitiveMatch = "JavaScript is fun".match(/javascript/i);
console.log(caseInsensitiveMatch[0]); // "JavaScript"
```

### การแทนที่ข้อความ
ใช้ `String.prototype.replace()` เพื่อแทนที่ส่วนของสตริง:

```javascript
const newStr = "JavaScript is fun".replace(/fun/, "awesome");
console.log(newStr); // "JavaScript is awesome"
```

### การใช้กลุ่ม
กลุ่มสามารถจับส่วนหนึ่งของรูปแบบ:

```javascript
const groupedPattern = /(\w+) is (\w+)/;
const replaceWithGroups = "JavaScript is fun".replace(groupedPattern, "$2 is $1");
console.log(replaceWithGroups); // "fun is JavaScript"
```

### ไลบรารีของบุคคลที่สาม
แม้ว่าความสามารถของ regex ที่มาพร้อมกับ JavaScript จะทรงพลัง, บางงานอาจถูกทำให้ง่ายขึ้นด้วยไลบรารีเช่น `XRegExp` มันมอบซินแทกซ์และธงเพิ่มเติม, ทำให้รูปแบบที่ซับซ้อนสามารถอ่านได้ง่ายขึ้น:

```javascript
// ตัวอย่างไลบรารี XRegExp
const XRegExp = require('xregexp');
const str = "Cats are fantastic.";
const unicodeWordMatch = XRegExp.match(str, XRegExp('\\p{L}+'), 'all');
console.log(unicodeWordMatch); // ["Cats", "are", "fantastic"]
```

ตัวอย่างนี้แสดงการใช้ `XRegExp` เพื่อจับคู่คำ Unicode ทั้งหมดในสตริง, ทำให้เห็นถึงความสามารถของไลบรารีในการจัดการชุดอักขระที่กว้างขวางกว่าที่ JavaScript มีในตัว

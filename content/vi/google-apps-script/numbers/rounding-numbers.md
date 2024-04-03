---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:01:29.986468-07:00
description: "C\xE1ch th\u1EE9c: Google Apps Script, v\u1EDBi t\u01B0 c\xE1ch l\xE0\
  \ ng\xF4n ng\u1EEF d\u1EF1a tr\xEAn JavaScript, cung c\u1EA5p c\xE1c ph\u01B0\u01A1\
  ng th\u1EE9c ti\xEAu chu\u1EA9n \u0111\u1EC3 l\xE0m tr\xF2n s\u1ED1. D\u01B0\u1EDB\
  i \u0111\xE2y l\xE0 c\xE1ch ph\xE2n t\xEDch\u2026"
lastmod: '2024-03-13T22:44:36.033075-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script, v\u1EDBi t\u01B0 c\xE1ch l\xE0 ng\xF4n ng\u1EEF d\u1EF1\
  a tr\xEAn JavaScript, cung c\u1EA5p c\xE1c ph\u01B0\u01A1ng th\u1EE9c ti\xEAu chu\u1EA9\
  n \u0111\u1EC3 l\xE0m tr\xF2n s\u1ED1."
title: "L\xE0m tr\xF2n s\u1ED1"
weight: 13
---

## Cách thức:
Google Apps Script, với tư cách là ngôn ngữ dựa trên JavaScript, cung cấp các phương thức tiêu chuẩn để làm tròn số. Dưới đây là cách phân tích ba kỹ thuật thường được sử dụng:

### Math.round()
Hàm này làm tròn một số tới số nguyên gần nhất.

```javascript
var number = 2.56;
var roundedNumber = Math.round(number); 
Logger.log(roundedNumber); // In ra: 3
```

### Math.ceil()
Làm tròn một số lên số nguyên gần nhất.

```javascript
var number = 2.56;
var roundedUp = Math.ceil(number); 
Logger.log(roundedUp); // In ra: 3
```

### Math.floor()
Ngược lại, làm tròn một số xuống số nguyên gần nhất.

```javascript
var number = 2.56;
var roundedDown = Math.floor(number); 
Logger.log(roundedDown); // In ra: 2
```

Đối với số lượng chỗ thập phân cụ thể, bạn có thể sử dụng `.toFixed()`, thực chất nó trả về một chuỗi, hoặc một cách tiếp cận tinh tế hơn cho việc làm tròn toán học:

```javascript
var number = 2.56789;
var fixedNumber = number.toFixed(2); 
Logger.log(fixedNumber); // In ra: "2.57" (dưới dạng chuỗi)

var preciseRound = Math.round(number * 100) / 100; 
Logger.log(preciseRound); // In ra: 2.57
```

## Sâu hơn
Việc làm tròn số trong Google Apps Script không khác nhiều so với cách thực hiện trong các môi trường JavaScript khác. Tuy nhiên, việc hiểu biết về sự khác biệt trong các phương thức làm tròn và tiềm năng của vấn đề số học dấu phẩy động là rất quan trọng. Ví dụ, do cách máy tính biểu diễn số dấu phẩy động, không phải tất cả các phân số thập phân có thể được biểu diễn một cách chính xác tuyệt đối, dẫn đến kết quả làm tròn đôi khi không như mong đợi.

Lịch sử, JavaScript (và theo đó, Google Apps Script) xử lý điều này bằng cách tuân theo tiêu chuẩn IEEE 754, được nhiều ngôn ngữ lập trình khác sử dụng cho số học dấu phẩy động. Tiêu chuẩn này định rõ cách số được làm tròn, đảm bảo sự nhất quán trên các nền tảng và ngôn ngữ khác nhau.

Mặc dù các phương pháp làm tròn trực tiếp trong Google Apps Script là đơn giản và thường đủ, các ứng dụng phức tạp hoặc yêu cầu độ chính xác cao có thể được hưởng lợi từ các thư viện như decimal.js hoặc big.js, được thiết kế để xử lý số học độ chính xác tùy ý. Điều này có thể đặc biệt hữu ích khi làm việc với các tính toán tài chính hoặc khoa học, nơi độ chính xác của các số được làm tròn là rất quan trọng.

Tuy nhiên, hãy nhớ rằng việc tận dụng các thư viện bên ngoài trong Google Apps Script đòi hỏi việc tải chúng thông qua trình chỉnh sửa script, có thể gây ra việc phụ thuộc hoặc ảnh hưởng đến hiệu suất của script tùy thuộc vào cách sử dụng. Trong nhiều trường hợp, các phương pháp Math có sẵn hoàn toàn đủ, nhưng đối với những trường hợp cụ thể đòi hỏi độ chính xác tới phần nghìn, việc nhìn ngoài thư viện tiêu chuẩn có thể là cần thiết.

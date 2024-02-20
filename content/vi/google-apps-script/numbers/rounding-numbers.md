---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:01:29.986468-07:00
description: "L\xE0m tr\xF2n s\u1ED1, m\u1ED9t kh\xE1i ni\u1EC7m c\u01A1 b\u1EA3n\
  \ trong l\u1EADp tr\xECnh m\xE1y t\xEDnh, bao g\u1ED3m vi\u1EC7c \u0111i\u1EC1u\
  \ ch\u1EC9nh m\u1ED9t s\u1ED1 \u0111\u1EC3 g\u1EA7n v\u1EDBi s\u1ED1 nguy\xEAn ho\u1EB7\
  c \u0111\u1EBFn m\u1ED9t s\u1ED1 l\u01B0\u1EE3ng ch\u1ED7 th\u1EADp ph\xE2n c\u1EE5\
  \u2026"
lastmod: 2024-02-19 22:04:55.221701
model: gpt-4-0125-preview
summary: "L\xE0m tr\xF2n s\u1ED1, m\u1ED9t kh\xE1i ni\u1EC7m c\u01A1 b\u1EA3n trong\
  \ l\u1EADp tr\xECnh m\xE1y t\xEDnh, bao g\u1ED3m vi\u1EC7c \u0111i\u1EC1u ch\u1EC9\
  nh m\u1ED9t s\u1ED1 \u0111\u1EC3 g\u1EA7n v\u1EDBi s\u1ED1 nguy\xEAn ho\u1EB7c \u0111\
  \u1EBFn m\u1ED9t s\u1ED1 l\u01B0\u1EE3ng ch\u1ED7 th\u1EADp ph\xE2n c\u1EE5\u2026"
title: "L\xE0m tr\xF2n s\u1ED1"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Làm tròn số, một khái niệm cơ bản trong lập trình máy tính, bao gồm việc điều chỉnh một số để gần với số nguyên hoặc đến một số lượng chỗ thập phân cụ thể. Lập trình viên thường thực hiện làm tròn số để đơn giản hóa các số cho độ dễ đọc của con người hoặc để đáp ứng nhu cầu tính toán cụ thể, đảm bảo độ chính xác và giảm tải tính toán.

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

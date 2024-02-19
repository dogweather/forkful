---
aliases:
- /vi/google-apps-script/concatenating-strings/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:50:36.674719-07:00
description: "N\u1ED1i chu\u1ED7i bao g\u1ED3m vi\u1EC7c k\u1EBFt h\u1EE3p hai ho\u1EB7\
  c nhi\u1EC1u chu\u1ED7i th\xE0nh m\u1ED9t chu\u1ED7i duy nh\u1EA5t. L\u1EADp tr\xEC\
  nh vi\xEAn th\u1EF1c hi\u1EC7n vi\u1EC7c n\xE0y \u0111\u1EC3 x\xE2y d\u1EF1ng \u0111\
  \u1ED9ng c\xE1c th\xF4ng \u0111i\u1EC7p, URL, ho\u1EB7c\u2026"
lastmod: 2024-02-18 23:08:50.210159
model: gpt-4-0125-preview
summary: "N\u1ED1i chu\u1ED7i bao g\u1ED3m vi\u1EC7c k\u1EBFt h\u1EE3p hai ho\u1EB7\
  c nhi\u1EC1u chu\u1ED7i th\xE0nh m\u1ED9t chu\u1ED7i duy nh\u1EA5t. L\u1EADp tr\xEC\
  nh vi\xEAn th\u1EF1c hi\u1EC7n vi\u1EC7c n\xE0y \u0111\u1EC3 x\xE2y d\u1EF1ng \u0111\
  \u1ED9ng c\xE1c th\xF4ng \u0111i\u1EC7p, URL, ho\u1EB7c\u2026"
title: "N\u1ED1i chu\u1ED7i k\xFD t\u1EF1"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Nối chuỗi bao gồm việc kết hợp hai hoặc nhiều chuỗi thành một chuỗi duy nhất. Lập trình viên thực hiện việc này để xây dựng động các thông điệp, URL, hoặc bất kỳ hình thức văn bản nào yêu cầu sự kết hợp của nội dung tĩnh và biến.

## Làm thế nào:

Trong Google Apps Script, dựa trên JavaScript, có một số cách để nối chuỗi. Dưới đây là một số phương pháp phổ biến:

### Sử dụng toán tử cộng (`+`):

```javascript
var firstName = "John";
var lastName = "Doe";
var fullName = firstName + " " + lastName;
Logger.log(fullName); // Output: John Doe
```

### Sử dụng phương thức `concat()`:

```javascript
var string1 = "Hello";
var string2 = "World";
var combinedString = string1.concat(" ", string2);
Logger.log(combinedString); // Output: Hello World
```

### Sử dụng template literals (dấu ngoặc nghịch):

Đây là cách hiện đại và linh hoạt để nối chuỗi, cho phép bạn dễ dàng nhúng biểu thức vào trong chuỗi.

```javascript
var language = "Google Apps Script";
var message = `Learning ${language} is fun!`;
Logger.log(message); // Output: Learning Google Apps Script is fun!
```

Mỗi phương pháp này có các trường hợp sử dụng của nó, và việc chọn phương pháp thường phụ thuộc vào yêu cầu về tính dễ đọc và độ phức tạp của các chuỗi được nối.

## Tìm hiểu sâu

Nối chuỗi là một khía cạnh cơ bản không chỉ trong Google Apps Script mà còn trong nhiều ngôn ngữ lập trình. Trong lịch sử, việc nối chuỗi thường được thực hiện sử dụng toán tử cộng hoặc các hàm/phương pháp chuyên biệt như `concat()`. Tuy nhiên, với sự ra đời của template literals trong ECMAScript 2015 (ES6), mà Google Apps Script hỗ trợ, các nhà phát triển đã có được một cách mạnh mẽ và trực quan hơn để xử lý chuỗi.

Template literals không chỉ đơn giản hóa cú pháp cho việc nhúng biểu thức vào trong chuỗi mà còn hỗ trợ chuỗi đa dòng mà không cần đến ký tự xuống dòng rõ ràng. Điều này giảm thiểu khả năng phát sinh lỗi và cải thiện tính dễ đọc của mã, đặc biệt khi xử lý với chuỗi phức tạp hoặc khi thay thế nhiều biến vào một mẫu văn bản.

Mặc dù toán tử `+` và phương pháp `concat()` vẫn được sử dụng rộng rãi và hỗ trợ cho khả năng tương thích ngược và đơn giản trong các tình huống đơn giản hơn, template literals cung cấp một lựa chọn hiện đại, biểu cảm được nhiều người coi là ưu việt hơn cho việc nối chuỗi, đặc biệt khi tính dễ đọc và bảo trì được quan tâm.

Tuy nhiên, quan trọng là phải chọn phương pháp phù hợp nhất với bối cảnh và yêu cầu cụ thể của dự án của bạn, xem xét các yếu tố như khả năng tương thích của môi trường mục tiêu (dù đây hiếm khi là vấn đề với Google Apps Script), hậu quả về hiệu năng (tối thiểu cho hầu hết các ứng dụng), và sự quen thuộc của nhóm phát triển với các tính năng JavaScript hiện đại.

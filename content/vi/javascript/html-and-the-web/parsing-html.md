---
changelog:
- 2024-01-28, dogweather, reviewed
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:02.179399-07:00
description: "Ph\xE2n t\xEDch c\xFA ph\xE1p HTML c\xF3 ngh\u0129a l\xE0 tr\xEDch xu\u1EA5\
  t d\u1EEF li\u1EC7u t\u1EEB t\xE0i li\u1EC7u HTML. C\xE1c l\u1EADp tr\xECnh vi\xEA\
  n th\u1EF1c hi\u1EC7n \u0111i\u1EC1u n\xE0y \u0111\u1EC3 t\u01B0\u01A1ng t\xE1c\
  \ v\u1EDBi ho\u1EB7c thao t\xE1c n\u1ED9i dung web,\u2026"
lastmod: 2024-02-19 22:04:56.359519
model: gpt-4-0125-preview
summary: "Ph\xE2n t\xEDch c\xFA ph\xE1p HTML c\xF3 ngh\u0129a l\xE0 tr\xEDch xu\u1EA5\
  t d\u1EEF li\u1EC7u t\u1EEB t\xE0i li\u1EC7u HTML. C\xE1c l\u1EADp tr\xECnh vi\xEA\
  n th\u1EF1c hi\u1EC7n \u0111i\u1EC1u n\xE0y \u0111\u1EC3 t\u01B0\u01A1ng t\xE1c\
  \ v\u1EDBi ho\u1EB7c thao t\xE1c n\u1ED9i dung web,\u2026"
title: "Ph\xE2n T\xEDch C\xFA Ph\xE1p HTML"
---

{{< edit_this_page >}}

## Cái Gì & Tại Sao?
Phân tích cú pháp HTML có nghĩa là trích xuất dữ liệu từ tài liệu HTML. Các lập trình viên thực hiện điều này để tương tác với hoặc thao tác nội dung web, tự động hóa việc trích xuất dữ liệu, hoặc cho mục đích web scraping.

## Làm Thế Nào:
Hãy phân tích cú pháp HTML sử dụng API `DOMParser` trong JavaScript.

```Javascript
const parser = new DOMParser();
const htmlString = `<p>Hello, world!</p>`;
const doc = parser.parseFromString(htmlString, 'text/html');
console.log(doc.body.textContent); // Đầu Ra: Hello, world!
```

Bây giờ, hãy thử lấy một cái gì đó cụ thể hơn, như một phần tử với một lớp:

```Javascript
const htmlString = `<div><p class="greeting">Hello, again!</p></div>`;
const doc = parser.parseFromString(htmlString, 'text/html');
const greeting = doc.querySelector('.greeting').textContent;
console.log(greeting); // Đầu Ra: Hello, again!
```

## Sâu Hơn
Phân tích cú pháp HTML cũng cổ xưa như web. Ban đầu, đó là chuyện của trình duyệt—các trình duyệt phân tích cú pháp HTML để hiển thị các trang web. Theo thời gian, các lập trình viên muốn tham gia vào quá trình này, dẫn đến việc ra đời các API như `DOMParser`.

Có phương án thay thế không? Chắc chắn rồi. Chúng ta có các thư viện như `jQuery` và các công cụ như `BeautifulSoup` cho Python. Nhưng `DOMParser` bản địa của JavaScript nhanh và đã được tích hợp sẵn, không cần thêm thư viện nào khác.

Về mặt thực hiện, khi bạn phân tích cú pháp HTML với `DOMParser`, nó tạo ra một đối tượng `Document`. Hãy nghĩ về nó như một mô hình phân cấp của HTML của bạn. Một khi bạn có nó, bạn có thể điều hướng và thao tác nó giống như bạn sẽ làm với DOM của một trang web bình thường.

Đây là điều—phân tích cú pháp có thể gặp vấn đề với HTML không đúng định dạng. Các trình duyệt có thể tha thứ, nhưng `DOMParser` có thể không. Do đó, đối với các nhiệm vụ phức tạp hoặc HTML lộn xộn, các thư viện bên thứ ba có thể làm sạch tốt hơn.

## Xem Thêm
- MDN Web Docs về API `DOMParser`: [MDN DOMParser](https://developer.mozilla.org/en-US/docs/Web/API/DOMParser)
- Khả năng phân tích cú pháp của jQuery: [jQuery.parseHTML()](https://api.jquery.com/jquery.parsehtml/)
- Cheerio, một triển khai nhanh chóng, linh hoạt và gọn nhẹ của core jQuery cho máy chủ: [Cheerio.js](https://cheerio.js.org/)
- Đối với phân tích cú pháp không phải JS: Thư viện BeautifulSoup của Python: [Beautiful Soup](https://www.crummy.com/software/BeautifulSoup/)

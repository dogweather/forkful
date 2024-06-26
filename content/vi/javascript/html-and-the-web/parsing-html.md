---
changelog:
- 2024-01-28, dogweather, reviewed
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:02.179399-07:00
description: "L\xE0m Th\u1EBF N\xE0o: H\xE3y ph\xE2n t\xEDch c\xFA ph\xE1p HTML s\u1EED\
  \ d\u1EE5ng API `DOMParser` trong JavaScript."
lastmod: '2024-03-13T22:44:37.150431-06:00'
model: gpt-4-0125-preview
summary: "H\xE3y ph\xE2n t\xEDch c\xFA ph\xE1p HTML s\u1EED d\u1EE5ng API `DOMParser`\
  \ trong JavaScript."
title: "Ph\xE2n T\xEDch C\xFA Ph\xE1p HTML"
weight: 43
---

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

---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:06:05.134173-07:00
description: "JSON, ho\u1EB7c JavaScript Object Notation, l\xE0 m\u1ED9t \u0111\u1ECB\
  nh d\u1EA1ng nh\u1EB9 cho vi\u1EC7c l\u01B0u tr\u1EEF v\xE0 v\u1EADn chuy\u1EC3\
  n d\u1EEF li\u1EC7u, l\xFD t\u01B0\u1EDFng cho vi\u1EC7c giao ti\u1EBFp gi\u1EEF\
  a m\xE1y ch\u1EE7 v\xE0 m\xE1y kh\xE1ch\u2026"
lastmod: '2024-02-25T18:49:34.443933-07:00'
model: gpt-4-0125-preview
summary: "JSON, ho\u1EB7c JavaScript Object Notation, l\xE0 m\u1ED9t \u0111\u1ECB\
  nh d\u1EA1ng nh\u1EB9 cho vi\u1EC7c l\u01B0u tr\u1EEF v\xE0 v\u1EADn chuy\u1EC3\
  n d\u1EEF li\u1EC7u, l\xFD t\u01B0\u1EDFng cho vi\u1EC7c giao ti\u1EBFp gi\u1EEF\
  a m\xE1y ch\u1EE7 v\xE0 m\xE1y kh\xE1ch\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi JSON"
---

{{< edit_this_page >}}

## Gì & Tại Sao?

JSON, hoặc JavaScript Object Notation, là một định dạng nhẹ cho việc lưu trữ và vận chuyển dữ liệu, lý tưởng cho việc giao tiếp giữa máy chủ và máy khách và các tệp cấu hình. Lập trình viên tận dụng nó trong Google Apps Script để trao đổi dữ liệu một cách liền mạch giữa các dịch vụ của Google (như Sheets, Docs, Drive) và các nguồn bên ngoài, do cấu trúc dễ đọc và tích hợp dễ dàng trong môi trường dựa trên JavaScript.

## Cách thức:

Trong Google Apps Script, việc thao tác JSON là một quá trình đơn giản, chủ yếu là do sự hỗ trợ tự nhiên mà JavaScript cung cấp cho việc phân tích cú pháp và biểu diễn chuỗi JSON. Dưới đây là một số thao tác phổ biến:

**1. Phân tích cú pháp JSON**: Giả sử chúng ta nhận một chuỗi JSON từ một dịch vụ web; việc phân tích nó thành một đối tượng JavaScript là thiết yếu cho việc thao tác dữ liệu.

```javascript
var jsonString = '{"name": "Sample Project", "version": "1.0.0"}';
var obj = JSON.parse(jsonString);
Logger.log(obj.name); // Kết quả: Sample Project
```

**2. Tạo chuỗi JSON từ Đối tượng JavaScript**: Ngược lại, việc chuyển đổi một đối tượng JavaScript thành một chuỗi JSON là hữu ích khi chúng ta cần gửi dữ liệu từ Apps Script tới một dịch vụ bên ngoài.

```javascript
var projectData = {
  name: "Sample Project",
  version: "1.0.0"
};
var jsonString = JSON.stringify(projectData);
Logger.log(jsonString); // Kết quả: '{"name":"Sample Project","version":"1.0.0"}'
```

**3. Làm việc với Dữ liệu Phức tạp**:
Đối với các cấu trúc dữ liệu phức tạp hơn, như mảng các đối tượng, quy trình vẫn giống nhau, thể hiện sự linh hoạt của JSON trong việc biểu diễn dữ liệu.

```javascript
var projects = [
  {name: "Project 1", version: "1.0"},
  {name: "Project 2", version: "2.0"}
];
var jsonString = JSON.stringify(projects);
Logger.log(jsonString); // Kết quả: '[{"name":"Project 1","version":"1.0"},{"name":"Project 2","version":"2.0"}]'
```

## Sâu xa hơn

Tính phổ biến của JSON trong các ứng dụng web hiện đại không thể phủ nhận được, gốc rễ trong sự đơn giản và cách nó tích hợp một cách liền mạch với JavaScript, ngôn ngữ của web. Thiết kế của nó, được truyền cảm hứng từ các đối tượng mẫu JavaScript, mặc dù nghiêm ngặt hơn, đã tạo điều kiện thuận lợi cho sự chấp nhận nhanh chóng của nó. Vào đầu những năm 2000, JSON đã trở nên phổ biến như một phương thức thay thế cho XML đối với các ứng dụng web sử dụng AJAX, cung cấp một định dạng trao đổi dữ liệu nhẹ hơn và ít dài dòng hơn. Với sự tích hợp sâu rộng của Google Apps Script với các API của Google và các dịch vụ bên ngoài, JSON đóng vai trò là một định dạng quan trọng cho việc cấu trúc, vận chuyển, và thao tác dữ liệu trên các nền tảng này.

Dù JSON chiếm ưu thế cho các ứng dụng web, các định dạng dữ liệu thay thế như YAML cho các tệp cấu hình hoặc Protobuf cho việc tuần tự hóa nhị phân hiệu quả hơn trong các môi trường hiệu năng cao tồn tại. Tuy nhiên, sự cân bằng của JSON giữa tính đọc được, dễ sử dụng và sự hỗ trợ rộng rãi trên các ngôn ngữ lập trình và công cụ đã củng cố vị thế của nó như là lựa chọn mặc định cho nhiều nhà phát triển tiếp cận với Google Apps Script và hơn thế nữa.

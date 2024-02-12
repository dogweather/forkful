---
title:                "Làm việc với JSON"
aliases:
- /vi/google-apps-script/working-with-json/
date:                  2024-02-01T22:06:05.134173-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/google-apps-script/working-with-json.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
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

---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:02:11.686776-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong Google Apps Script, \u0111\u1EC3 g\u1EED\
  i m\u1ED9t y\xEAu c\u1EA7u HTTP v\u1EDBi x\xE1c th\u1EF1c c\u01A1 b\u1EA3n, b\u1EA1\
  n s\u1EED d\u1EE5ng d\u1ECBch v\u1EE5 `UrlFetchApp` k\u1EBFt h\u1EE3p v\u1EDBi ti\xEA\
  u \u0111\u1EC1 \u1EE7y quy\u1EC1n \u0111\u01B0\u1EE3c m\xE3\u2026"
lastmod: '2024-03-13T22:44:36.039786-06:00'
model: gpt-4-0125-preview
summary: "Trong Google Apps Script, \u0111\u1EC3 g\u1EEDi m\u1ED9t y\xEAu c\u1EA7\
  u HTTP v\u1EDBi x\xE1c th\u1EF1c c\u01A1 b\u1EA3n, b\u1EA1n s\u1EED d\u1EE5ng d\u1ECB\
  ch v\u1EE5 `UrlFetchApp` k\u1EBFt h\u1EE3p v\u1EDBi ti\xEAu \u0111\u1EC1 \u1EE7\
  y quy\u1EC1n \u0111\u01B0\u1EE3c m\xE3 h\xF3a base64."
title: "G\u1EEDi y\xEAu c\u1EA7u HTTP v\u1EDBi x\xE1c th\u1EF1c c\u01A1 b\u1EA3n"
weight: 45
---

## Làm thế nào:
Trong Google Apps Script, để gửi một yêu cầu HTTP với xác thực cơ bản, bạn sử dụng dịch vụ `UrlFetchApp` kết hợp với tiêu đề ủy quyền được mã hóa base64. Dưới đây là hướng dẫn từng bước:

1. **Mã hóa Thông tin đăng nhập**: Đầu tiên, mã hóa tên đăng nhập và mật khẩu của bạn sang base64. Google Apps Script không có hàm mã hóa base64 cho chuỗi một cách tự nhiên, vì vậy bạn sẽ sử dụng Utilities.base64Encode cho mục đích này.

```javascript
var username = 'YourUsername';
var password = 'YourPassword';
var encodedCredentials = Utilities.base64Encode(username + ':' + password);
```

2. **Thiết lập Tùy chọn Yêu cầu**: Với thông tin đăng nhập đã mã hóa, chuẩn bị đối tượng tùy chọn cho yêu cầu HTTP, bao gồm phương thức và tiêu đề.

```javascript
var options = {
  method: 'get', // hoặc 'post', 'put', tùy thuộc vào nhu cầu của bạn
  headers: {
    'Authorization': 'Basic ' + encodedCredentials
  }
  // các tùy chọn bổ sung như 'muteHttpExceptions' cho việc xử lý lỗi có thể được thêm vào đây
};
```

3. **Thực hiện Yêu cầu**: Sử dụng phương thức `UrlFetchApp.fetch` với URL mục tiêu và đối tượng tùy chọn.

```javascript
var url = 'https://example.com/api/resource';
var response = UrlFetchApp.fetch(url, options);
Logger.log(response.getContentText());
```

Mẫu đầu ra khi yêu cầu thành công sẽ thay đổi dựa trên phản hồi của API. Đối với một API dựa trên JSON, bạn có thể thấy điều gì đó như:

```
{"status":"Success","data":"Resource data here..."}
```

Đảm bảo bạn xử lý các lỗi HTTP có thể xảy ra bằng cách kiểm tra mã phản hồi hoặc sử dụng tùy chọn `muteHttpExceptions` cho quản lý lỗi kiểm soát tốt hơn.

## Sâu hơn
Gửi một yêu cầu HTTP với xác thực cơ bản đã là phương pháp tiêu chuẩn trong nhiều ngôn ngữ lập trình để truy cập vào các tài nguyên dựa trên web yêu cầu xác thực. Trong bối cảnh của Google Apps Script, `UrlFetchApp` cung cấp một cách thẳng thắn để thực hiện các yêu cầu HTTP này, bao gồm cả những yêu cầu đòi hỏi xác thực. Việc đưa thông tin đăng nhập cơ bản vào tiêu đề yêu cầu là một phương pháp đơn giản nhưng hiệu quả; tuy nhiên, nó đi kèm với những rủi ro về bảo mật, chủ yếu vì thông tin đăng nhập được gửi dưới dạng bản rõ, chỉ được mã hóa base64, có thể dễ dàng giải mã nếu bị chặn lại.

Để cải thiện bảo mật, các phương án thay thế như OAuth 2.0 được khuyến nghị, đặc biệt là khi xử lý dữ liệu hoặc các hoạt động nhạy cảm. Google Apps Script hỗ trợ sẵn OAuth 2.0 với thư viện `OAuth2`, đơn giản hóa quá trình xác thực chống lại các dịch vụ hỗ trợ giao thức này.

Mặc dù có những hạn chế về bảo mật, xác thực cơ bản vẫn được sử dụng rộng rãi cho các ứng dụng đơn giản hoặc nội bộ không tiếp xúc rộng rãi với internet lớn. Nó dễ dàng được triển khai khi chỉ cần một yêu cầu duy nhất với các tiêu đề được thiết lập đúng cách, làm cho nó trở thành một phương án hấp dẫn cho các tích hợp nhanh chóng hoặc cho các API không có phương pháp bảo mật cao hơn. Tuy nhiên, lập trình viên được khuyến khích cân nhắc đến những hệ lụy về bảo mật và tìm kiếm các phương án an toàn hơn khi có sẵn.

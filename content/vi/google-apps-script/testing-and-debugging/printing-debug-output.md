---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:59:00.754850-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Google Apps Script cung c\u1EA5p l\u1EDB\
  p `Logger` cho vi\u1EC7c g\u1EE1 r\u1ED1i c\u01A1 b\u1EA3n, v\xE0 cho nhu c\u1EA7\
  u n\xE2ng cao h\u01A1n, l\u1EDBp `console` \u0111\u01B0\u1EE3c gi\u1EDBi thi\u1EC7\
  u trong th\u1EDDi\u2026"
lastmod: '2024-03-13T22:44:36.043987-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script cung c\u1EA5p l\u1EDBp `Logger` cho vi\u1EC7c g\u1EE1\
  \ r\u1ED1i c\u01A1 b\u1EA3n, v\xE0 cho nhu c\u1EA7u n\xE2ng cao h\u01A1n, l\u1EDB\
  p `console` \u0111\u01B0\u1EE3c gi\u1EDBi thi\u1EC7u trong th\u1EDDi gian ch\u1EA1\
  y V8."
title: "In ra th\xF4ng tin debug"
weight: 33
---

## Cách thực hiện:
Google Apps Script cung cấp lớp `Logger` cho việc gỡ rối cơ bản, và cho nhu cầu nâng cao hơn, lớp `console` được giới thiệu trong thời gian chạy V8.

**Sử dụng Logger:**

Lớp Logger cho phép bạn ghi nhật ký các thông báo gỡ rối, mà sau đó bạn có thể xem trong Apps Script Editor dưới `View > Logs`. Dưới đây là một ví dụ đơn giản:

```javascript
function logSample() {
  var name = "Người Đọc Wired";
  Logger.log("Chào, %s!", name);
}
```

Sau khi chạy `logSample()`, bạn có thể xem nhật ký với "Hello, Người Đọc Wired!" trong người xem Logs.

**Sử dụng console.log với thời gian chạy V8:**

Với thời gian chạy V8, `console.log` cung cấp cú pháp quen thuộc hơn cho các nhà phát triển đến từ các ngôn ngữ khác:

```javascript
function consoleSample() {
  var status = 'hoạt động';
  var count = 150;
  console.log(`Trạng thái hiện tại: ${status}, Số đếm: ${count}`);
}
```

Sau khi thực thi, truy cập Stackdriver Logging trong `View > Stackdriver Logging` để xem đầu ra. Nó mạnh mẽ hơn, hỗ trợ nội suy chuỗi và kiểm tra đối tượng, và tích hợp với việc ghi nhật ký của Google Cloud, cung cấp nhật ký lâu dài và các khả năng lọc nâng cao.

**Mẫu Đầu ra từ console.log:**

```
Trạng thái hiện tại: hoạt động, Số đếm: 150
```

## Sâu xa hơn
Ban đầu, `Logger.log` là công cụ chính cho việc gỡ lỗi trong Google Apps Script, cung cấp một cách đơn giản, trực tiếp để in đầu ra để kiểm tra. Tuy nhiên, khi các kịch bản trở nên phức tạp hơn và được tích hợp với các dịch vụ của Google Cloud Platform, nhu cầu về một giải pháp ghi nhật ký mạnh mẽ hơn trở nên rõ ràng.

Nhập thời gian chạy V8, mang `console.log` vào sử dụng. Điều này không chỉ đồng nhất Google Apps Script với cú pháp JavaScript chuẩn, làm cho ngôn ngữ này dễ tiếp cận hơn với các nhà phát triển quen với JavaScript mà còn tận dụng cơ sở hạ tầng mạnh mẽ của khả năng ghi nhật ký của Google Cloud. Sự ra đời của `console.log` và sự tích hợp của nó với Google Cloud Platform đánh dấu một bước tiến đáng kể trong khả năng gỡ lỗi trong Google Apps Script, cung cấp cho các nhà phát triển một cách tiếp cận động và có thể mở rộng hơn để giám sát và khắc phục sự cố các kịch bản của họ.

Mặc dù `Logger.log` đủ cho nhu cầu gỡ lỗi cơ bản và các dự án nhỏ, `console.log` với thời gian chạy V8 cung cấp một giải pháp tổng quát và chuẩn bị cho tương lai hơn. Điều này bao gồm khả năng giữ lại nhật ký sau phiên thực thi, tìm kiếm và lọc nhật ký trong bảng điều khiển Google Cloud, và sự phù hợp chung với các thực hành phát triển JavaScript hiện đại. Tuy nhiên, các nhà phát triển nên cân nhắc nhu cầu của họ so với độ phức tạp và quy mô của dự án của họ khi chọn giữa các lựa chọn này.

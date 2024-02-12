---
title:                "In ra thông tin debug"
aliases:
- /vi/google-apps-script/printing-debug-output.md
date:                  2024-02-01T21:59:00.754850-07:00
model:                 gpt-4-0125-preview
simple_title:         "In ra thông tin debug"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/google-apps-script/printing-debug-output.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại Sao?

In thông tin lỗi để gỡ rối bao gồm việc đặt chiến lược các câu lệnh ghi nhật ký trong mã của bạn để hiển thị giá trị biến, dòng thực thi, hoặc thông báo lỗi trong thời gian chạy. Lập trình viên sử dụng nó rộng rãi để truy vết và chẩn đoán hành vi của các kịch bản của họ, đảm bảo tính chính xác và hiệu quả trong các ứng dụng Google Apps Script của họ.

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

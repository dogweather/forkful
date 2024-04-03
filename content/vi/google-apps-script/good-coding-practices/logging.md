---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:57:10.623201-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong Google Apps Script, logging c\xF3 th\u1EC3\
  \ \u0111\u01B0\u1EE3c th\u1EF1c hi\u1EC7n b\u1EB1ng c\xE1c ph\u01B0\u01A1ng ph\xE1\
  p kh\xE1c nhau, ch\u1EB3ng h\u1EA1n nh\u01B0 l\u1EDBp `Logger` v\xE0 `console.log()`.\
  \ L\u1EDBp\u2026"
lastmod: '2024-03-13T22:44:36.049121-06:00'
model: gpt-4-0125-preview
summary: "Trong Google Apps Script, logging c\xF3 th\u1EC3 \u0111\u01B0\u1EE3c th\u1EF1\
  c hi\u1EC7n b\u1EB1ng c\xE1c ph\u01B0\u01A1ng ph\xE1p kh\xE1c nhau, ch\u1EB3ng h\u1EA1\
  n nh\u01B0 l\u1EDBp `Logger` v\xE0 `console.log()`."
title: "Ghi nh\u1EADt k\xFD"
weight: 17
---

## Làm thế nào:
Trong Google Apps Script, logging có thể được thực hiện bằng các phương pháp khác nhau, chẳng hạn như lớp `Logger` và `console.log()`. Lớp Logger là cách truyền thống, phù hợp cho việc gỡ lỗi và mục đích phát triển đơn giản. Tính đến những bản cập nhật gần đây, `console.log()` cung cấp nhiều tính linh hoạt và tích hợp với Stackdriver Logging, cung cấp một giải pháp mạnh mẽ hơn để giám sát Apps Scripts của bạn trên Google Cloud Platform.

**Sử dụng Logger:**

```javascript
function logSample() {
  Logger.log('Đây là một thông điệp log đơn giản');
  
  var value = 5;
  Logger.log('Giá trị là: %s', value); // Định dạng chuỗi
}

// Để xem log:
// 1. Chạy hàm logSample.
// 2. Xem -> Logs
```

**Kết Quả Logger Mẫu:**

```
[22-04-20 10:00:00:000 PDT] Đây là một thông điệp log đơn giản
[22-04-20 10:00:00:001 PDT] Giá trị là: 5
```

**Sử dụng console.log():**

```javascript
function consoleLogSample() {
  console.log('Thông điệp này đến với Stackdriver Logging');
  const obj = {name: 'Jane', role: 'Developer'};
  console.info('Logging một đối tượng:', obj);
}

// Logs có thể được xem trong bảng điều khiển của Google Cloud Platform (GCP) dưới Stackdriver Logging
```

**Kết Quả console.log() Mẫu:**

```
Thông điệp này đến với Stackdriver Logging
Logging một đối tượng: {name: "Jane", role: "Developer"}
```

Bằng cách chuyển sang `console.log()` cho các ứng dụng phức tạp, các nhà phát triển có thể hiệu quả phân tích và phân tích các log bằng cách sử dụng các bộ lọc và công cụ mạnh mẽ được cung cấp bởi GCP, điều này không dễ dàng với lớp Logger truyền thống.

## Sâu Hơn:
Logging trong Google Apps Script đã phát triển đáng kể. Ban đầu, lớp `Logger` là phương thức chính mà các nhà phát triển sử dụng để gỡ lỗi cho các script của họ. Nó đơn giản và đủ cho các script cơ bản, nhưng nó thiếu các khả năng cần thiết cho các ứng dụng đám mây hiện đại, chẳng hạn như tìm kiếm logs hoặc phân tích xu hướng log theo thời gian.

Giới thiệu `console.log()` đã cầu nối khoảng cách này bằng cách tích hợp ghi log Google Apps Script với Stackdriver Logging của Google Cloud (nay được gọi là Operations Suite), cung cấp một nền tảng trung tâm cho việc ghi log, giám sát và gỡ lỗi các ứng dụng. Điều này không chỉ cho phép ghi log ở quy mô mà còn mở ra các tính năng quản lý log tiên tiến như số liệu dựa trên log, phân tích log thời gian thực, và tích hợp với các dịch vụ Google Cloud khác.

Trong khi `Logger` vẫn phục vụ một mục đích cho việc gỡ lỗi nhanh chóng và log trong các script nhỏ, sự phát triển về việc sử dụng `console.log()` phản ánh sự chuyển dịch rộng lớn hơn trong phát triển các ứng dụng dựa trên đám mây đáng mở rộng. Điều này nhấn mạnh cam kết của Google trong việc cung cấp các công cụ phù hợp với độ phức tạp và quy mô của các ứng dụng hiện nay. Tuy nhiên, người mới bắt đầu nên lưu ý về việc học khá khắc nghiệt và sự cần thiết phải làm quen với các khái niệm của Google Cloud Platform. Mặc dù vậy, việc chuyển đổi này mang lại lợi ích cho các nhà phát triển muốn tận dụng triệt để các khả năng của đám mây. Sự phù hợp này với các dịch vụ đám mây là một phần của xu hướng rộng lớn hơn trong phát triển phần mềm, nhấn mạnh tầm quan trọng của cơ chế log mạnh mẽ, có thể mở rộng trong kỷ nguyên của điện toán đám mây.

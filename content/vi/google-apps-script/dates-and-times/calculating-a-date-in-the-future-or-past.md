---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:49:38.924364-07:00
description: "Vi\u1EC7c t\xEDnh to\xE1n m\u1ED9t ng\xE0y trong t\u01B0\u01A1ng lai\
  \ ho\u1EB7c qu\xE1 kh\u1EE9 li\xEAn quan \u0111\u1EBFn vi\u1EC7c thao t\xE1c v\u1EDB\
  i c\xE1c \u0111\u1ED1i t\u01B0\u1EE3ng ng\xE0y \u0111\u1EC3 t\xECm ra c\xE1c ng\xE0\
  y n\u1EB1m ngo\xE0i ho\u1EB7c tr\u01B0\u1EDBc ng\xE0y hi\u1EC7n\u2026"
lastmod: '2024-03-13T22:44:36.058862-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c t\xEDnh to\xE1n m\u1ED9t ng\xE0y trong t\u01B0\u01A1ng lai ho\u1EB7\
  c qu\xE1 kh\u1EE9 li\xEAn quan \u0111\u1EBFn vi\u1EC7c thao t\xE1c v\u1EDBi c\xE1\
  c \u0111\u1ED1i t\u01B0\u1EE3ng ng\xE0y \u0111\u1EC3 t\xECm ra c\xE1c ng\xE0y n\u1EB1\
  m ngo\xE0i ho\u1EB7c tr\u01B0\u1EDBc ng\xE0y hi\u1EC7n\u2026"
title: "T\xEDnh to\xE1n m\u1ED9t ng\xE0y trong t\u01B0\u01A1ng lai ho\u1EB7c qu\xE1\
  \ kh\u1EE9"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Việc tính toán một ngày trong tương lai hoặc quá khứ liên quan đến việc thao tác với các đối tượng ngày để tìm ra các ngày nằm ngoài hoặc trước ngày hiện tại. Các lập trình viên làm việc này cho các nhiệm vụ từ việc thiết lập lời nhắc đến xác định ngày hết hạn hoặc phân tích xu hướng dữ liệu theo thời gian.

## Làm thế nào:

Trong Google Apps Script, dựa trên JavaScript, bạn có thể thao tác với ngày bằng cách sử dụng đối tượng `Date`. Dưới đây là cách để tính toán ngày trong tương lai và quá khứ:

### Tính Ngày Tương Lai

Để tính một ngày trong tương lai, bạn tạo một đối tượng ngày cho ngày hiện tại và sau đó thêm số ngày mong muốn (hoặc bất kỳ đơn vị thời gian nào khác) vào nó.

```javascript
// Ngày hiện tại
var today = new Date();

// Tính một ngày 10 ngày trong tương lai
var futureDate = new Date(today);
futureDate.setDate(today.getDate() + 10);

Logger.log("Ngày Tương Lai: " + futureDate.toDateString());
```

### Tính Ngày Quá Khứ

Tương tự, để tìm một ngày trong quá khứ, bạn trừ đi số ngày từ ngày hiện tại.

```javascript
// Ngày hiện tại
var today = new Date();

// Tính một ngày 10 ngày trong quá khứ
var pastDate = new Date(today);
pastDate.setDate(today.getDate() - 10);

Logger.log("Ngày Quá Khứ: " + pastDate.toDateString());
```

### Mẫu Đầu Ra

Điều này sẽ xuất ra một cái gì đó giống như sau (giả sử hôm nay là 15 tháng 4, 2023):

```
Ngày Tương Lai: Thứ Ba 25 tháng 4 2023
Ngày Quá Khứ: Thứ Tư 05 tháng 4 2023
```

Nhớ rằng, đối tượng Date trong JavaScript (và do đó trong Google Apps Script) tự động điều chỉnh các tháng và năm khi bạn thêm hoặc trừ ngày.

## Sâu xa hơn

Sự thao tác với ngày sử dụng đối tượng `Date` bắt nguồn từ các triển khai JavaScript ban đầu. Theo thời gian, phương pháp này về cơ bản vẫn giữ nguyên, cung cấp một cách trực tiếp cho các nhà phát triển để quản lý ngày mà không cần đến các thư viện bên ngoài. Tuy nhiên, đối với các thao tác phức tạp hơn như điều chỉnh múi giờ, hoặc khi làm việc với dữ liệu dựa trên ngày lớn, các thư viện như `Moment.js` hoặc `Luxon` hiện đại hơn có thể cung cấp nhiều chức năng và dễ dàng hơn trong quản lý.

Cụ thể trong Google Apps Script, mặc dù có sẵn tính đơn giản và trực tiếp của đối tượng `Date`, điều quan trọng là phải suy nghĩ cẩn thận về cách tính toán ngày có thể ảnh hưởng đến hiệu suất và thời gian thực thi của script, đặc biệt trong các kích hoạt dựa trên thời gian hoặc thao tác bảng tính rộng rãi. Thêm nữa, mặc dù Google Apps Script cung cấp phương thức tích hợp sẵn để xử lý các ngày trong hệ sinh thái của nó (như trong Google Sheets hoặc Lịch), việc tích hợp các thư viện bên ngoài hoặc tận dụng các Dịch vụ Nâng cao của Google đôi khi có thể cung cấp các giải pháp mạnh mẽ hơn cho các kịch bản phức tạp.

Vậy, trong khi phương pháp sử dụng đối tượng `Date` của JavaScript bản địa thường đủ cho các tính toán đơn giản, khám phá các thư viện hoặc dịch vụ bên ngoài có thể nâng cao chức năng cho các yêu cầu tinh vi hơn.

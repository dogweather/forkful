---
title:                "So sánh hai ngày"
date:                  2024-01-28T21:57:30.229511-07:00
model:                 gpt-4-0125-preview
simple_title:         "So sánh hai ngày"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/gleam/comparing-two-dates.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

So sánh hai ngày là kiểm tra chúng liên quan với nhau trong thời gian như thế nào. Lập trình viên thực hiện việc này để sắp xếp sự kiện, thực hiện lịch trình, hoặc xác nhận các khoảng thời gian.

## Làm thế nào:

Trong Gleam, chúng ta sử dụng thư viện `gleam/calendar` cho việc xử lý ngày. Thật không may, tính đến đầu năm 2023 theo kiến thức của tôi, Gleam không có cách tích hợp sẵn để so sánh trực tiếp các ngày như một số ngôn ngữ khác có thể. Vậy nên, mặc dù không thể sử dụng ngay lập tức, nhưng với một vài hàm, chúng ta có thể bắt đầu so sánh ngày.

Trước tiên, hãy chắc chắn rằng chúng ta có thể tạo ra một số ngày:

```gleam
import gleam/calendar.{Date}

pub fn make_date(nam: Int, thang: Int, ngay: Int) -> Option(Date) {
  calendar.new_date(nam, thang, ngay)
}
```

Bây giờ, hãy viết một hàm để so sánh hai ngày. Chúng ta có thể chuyển đổi ngày sang một định dạng có thể so sánh - như số ngày kể từ một ngày đặt trước. Nhưng vì đây là một ví dụ đơn giản, chúng ta chỉ cần kiểm tra xem một ngày có trước ngày kia không:

```gleam
import gleam/calendar.{Date, is_before}

pub fn is_date1_before_date2(date1: Date, date2: Date) -> Bool {
  is_before(date1, date2)
}
```

Ví dụ sử dụng:

```gleam
import gleam/io

fn main() {
  let date1 = make_date(2023, 3, 14)
  let date2 = make_date(2021, 6, 18)
  
  let ketqua = case date1 {
    Ok(d1) -> case date2 {
      Ok(d2) -> is_date1_before_date2(d1, d2)
      Error(_) -> False
    }
    Error(_) -> False
  }
  
  io.debug(ketqua) // Nên in ra True vì date1 sau date2
}
```

## Tìm hiểu sâu

Trong lịch sử, các API ngày/giờ khác nhau giữa các ngôn ngữ, với một số cung cấp các toán tử so sánh mạnh mẽ và những ngôn ngữ khác yêu cầu tính toán bằng tay. Khi so sánh ngày, nhiều ngôn ngữ chuyển đổi sang một dạng chuẩn hóa như Unix time (giây kể từ ngày 1 tháng 1 năm 1970) có thể được so sánh trực tiếp. Tuy nhiên, các trường hợp ngoại lệ như giây nhuận hoặc giờ ánh sáng ban ngày có thể thêm phức tạp.

Trong Gleam, do trọng tâm của ngôn ngữ vào sự an toàn và đáng tin cậy, các hoạt động ngày có thể kém trực tiếp nhưng mục tiêu là chính xác mà không có giả định ngầm. Đó là lý do tại sao bạn có thể không tìm thấy một dòng lệnh để làm công việc này, nhưng với việc xử lý ngày bằng module `calendar` đúng cách, bạn có thể quản lý tốt.

Đối với các phương án khác, người ta có thể viết các hàm phức tạp hơn so sánh năm, sau đó là tháng, rồi ngày, nếu cần kiểm soát tinh vi hoặc cho đến khi hỗ trợ so sánh ngày trực tiếp thực sự được thêm vào Gleam. Cuối cùng, luôn cập nhật với những bản cập nhật ngôn ngữ; Gleam đang phát triển nhanh chóng, và các tính năng mới có thể xuất hiện sau khi tôi cập nhật kiến thức.

## Xem thêm

- Để biết hướng dẫn tổng thể về lang Gleam, hãy truy cập: [https://gleam.run](https://gleam.run).
- Để tìm hiểu về các vấn đề về xử lý thời gian và ngày trong lập trình và giải pháp của chúng, hãy đọc qua [https://yourcalendricalfallacyis.com/](https://yourcalendricalfallacyis.com/).

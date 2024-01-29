---
title:                "Tính toán ngày trong tương lai hoặc quá khứ"
date:                  2024-01-28T21:55:48.483117-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tính toán ngày trong tương lai hoặc quá khứ"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/gleam/calculating-a-date-in-the-future-or-past.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?
Việc tính toán một ngày trong tương lai hoặc quá khứ có nghĩa là xác định ngày sẽ là bao nhiêu sau hoặc trước một khoảng thời gian nhất định. Lập trình viên làm điều này cho các mục đích như lên lịch cho sự kiện, tái tục đăng ký, hoặc thiết lập ngày hết hạn.

## Làm thế nào:
Gleam không đi kèm với một thư viện ngày/giờ được xây dựng sẵn, vì vậy chúng ta sẽ sử dụng thư viện `chronotope` để xử lý ngày và giờ. Đầu tiên, thêm `chronotope` vào `gleam.toml` của bạn:

```toml
[dependencies]
chronotope = "~> 0.4"
```

Bây giờ, hãy thực hiện một số tính toán ngày:

```gleam
import chronotope
import chronotope.duration
import chronotope.date

fn calculate_date() {
  let now = chronotope.now()
  let two_weeks = duration.of_weeks(2)
  let future_date = date.add(now, two_weeks)
  let past_date = date.subtract(now, two_weeks)
  future_date, past_date
}

fn main() {
  let (future, past) = calculate_date()
  io.println(future)
  io.println(past)
}
```

Chạy nó:

```bash
$ gleam run
```

Mẫu đầu ra có thể là:

```
2023-04-28
2023-03-31
```

## Sâu hơn nữa
Trong máy tính, việc thao tác ngày là một phần của lĩnh vực rộng lớn hơn về cơ sở dữ liệu tạm thời và dữ liệu dựa trên thời gian. Trong thập niên 1970, với máy tính mainframe là điểm tựa chính, việc theo dõi ngày và giờ chính xác đã trở nên thiết yếu cho các chức năng như lên lịch công việc.

Về các phương án thay thế, trong khi `chronotope` là một lựa chọn vững chắc trong Gleam, các ngôn ngữ khác có thể sử dụng các thư viện tiêu chuẩn như `datetime` của Python hoặc đối tượng `Date` của JavaScript. Sự thực hiện trong các ngôn ngữ khác nhau có sự khác biệt, nhưng đa số chúng tính toán ngày bằng cách thao tác số mili giây kể từ một điểm epoch được biết đến (thường là ngày 1 tháng 1 năm 1970, UTC).

Bên trong, `chronotope` quản lý ngày như các struct và thực hiện các phép tính bằng cách chuyển đổi các khoảng thời gian thành đơn vị tương thích (ví dụ, giây hoặc ngày), thực hiện phép toán lên nó, và chuyển đổi trở lại thành struct ngày hoặc giờ. Quá trình này tính đến những tập tính kỳ quái trong lịch và múi giờ, không phải lúc nào cũng tuyến tính hoặc nhất quán do các năm nhuận, giờ tiết kiệm ánh sáng, và các ngoại lệ khác.

## Xem thêm
- [Cơ sở dữ liệu tạm thời trên Wikipedia](https://en.wikipedia.org/wiki/Temporal_database)
- [Lịch sử của Thiết bị Đo thời gian](https://en.wikipedia.org/wiki/History_of_timekeeping_devices)

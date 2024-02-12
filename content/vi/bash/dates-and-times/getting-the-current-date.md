---
title:                "Lấy ngày hiện tại"
aliases:
- /vi/bash/getting-the-current-date/
date:                  2024-01-28T22:01:41.450135-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lấy ngày hiện tại"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/bash/getting-the-current-date.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?
Lấy ngày hiện tại trong bash là việc lấy ngày và giờ của hệ thống. Lập trình viên cần nó cho việc ghi nhật ký, đánh dấu thời gian, hoặc lên lịch các tác vụ.

## Cách thức:
Để lấy ngày và giờ hiện tại, bạn sử dụng `date`. Đây là cách đơn giản:

```Bash
date
```

Và bùm, bạn có được cái gì đó như thế này:

```
Mon Mar 27 12:45:21 PDT 2023
```

Cần ngày ở định dạng khác? Không thành vấn đề. Sử dụng các tùy chọn `+%`:

```Bash
date +"%Y-%m-%d"
```

Kết quả giờ đã gọn gàng, ngăn nắp:

```
2023-03-27
```

## Sâu hơn nữa
Ngày xưa, các hệ thống không phải lúc nào cũng có đồng hồ nội bộ. Do đó, mọi người phụ thuộc vào hệ thống chia sẻ thời gian để lấy giờ. Ngày nay, mọi hệ thống bạn chạy Bash đều biết thời gian. Cảm ơn `date`.

`date` rất linh hoạt. Muốn lấy ngày của tuần sau? Chỉ cần thêm cờ `--date` đầy màu sắc:

```Bash
date --date="next week"
```

Nhưng chờ đã, còn nữa đấy! Có một múi giờ khác trong tâm trí?

```Bash
TZ="Europe/Paris" date
```

Bây giờ bạn đang lấy thời gian Paris. Thật là tuyệt.

Bash không đơn độc trong trò chơi lấy ngày. Python, PHP, JavaScript – tất cả đều có cách riêng của họ. Nhưng trong lĩnh vực viết kịch bản shell, `date` là người bạn đồng hành đáng tin cậy của bạn.

Tại sao điều này quan trọng? Tự động hóa, bạn của tôi. Các kịch bản làm việc tùy thuộc vào ngày và giờ dựa trên `date`. Các công việc Cron? Chúng yêu một dấu thời gian tốt.

Đây là bản tóm tắt kỹ thuật: `date` rút thông tin từ đồng hồ hệ thống, được đồng bộ hóa với các nguồn thời gian phần cứng hoặc mạng, vì vậy bạn không sống trong quá khứ.

## Xem thêm
- Kiểm tra `man date` để đọc một trải nghiệm thú vị về tất cả mọi thứ `date`.
- Ghé qua Wiki của Greg để có một số mẹo lập trình bash đầy ngẫu hứng: http://mywiki.wooledge.org/BashGuide
- Nếu bạn muốn tìm hiểu thêm, luôn có sổ tay GNU coreutils: https://www.gnu.org/software/coreutils/manual/coreutils.html#date-invocation

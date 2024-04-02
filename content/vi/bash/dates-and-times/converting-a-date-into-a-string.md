---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:33.547736-07:00
description: "Chuy\u1EC3n \u0111\u1ED5i m\u1ED9t ng\xE0y th\xE0nh chu\u1ED7i ngh\u0129\
  a l\xE0 chuy\u1EC3n n\xF3 th\xE0nh v\u0103n b\u1EA3n bi\u1EC3u di\u1EC5n ng\xE0\
  y \u0111\xF3. Ch\xFAng ta th\u1EF1c hi\u1EC7n vi\u1EC7c n\xE0y \u0111\u1EC3 \u0111\
  \u1ECBnh d\u1EA1ng c\xE1c ng\xE0y cho con ng\u01B0\u1EDDi d\u1EC5 \u0111\u1ECDc\u2026"
lastmod: '2024-03-13T22:44:36.894032-06:00'
model: gpt-4-0125-preview
summary: "Chuy\u1EC3n \u0111\u1ED5i m\u1ED9t ng\xE0y th\xE0nh chu\u1ED7i ngh\u0129\
  a l\xE0 chuy\u1EC3n n\xF3 th\xE0nh v\u0103n b\u1EA3n bi\u1EC3u di\u1EC5n ng\xE0\
  y \u0111\xF3. Ch\xFAng ta th\u1EF1c hi\u1EC7n vi\u1EC7c n\xE0y \u0111\u1EC3 \u0111\
  \u1ECBnh d\u1EA1ng c\xE1c ng\xE0y cho con ng\u01B0\u1EDDi d\u1EC5 \u0111\u1ECDc\u2026"
title: "Chuy\u1EC3n \u0111\u1ED5i m\u1ED9t ng\xE0y th\xE0nh chu\u1ED7i"
weight: 28
---

## Cái gì và Tại sao?
Chuyển đổi một ngày thành chuỗi nghĩa là chuyển nó thành văn bản biểu diễn ngày đó. Chúng ta thực hiện việc này để định dạng các ngày cho con người dễ đọc hoặc để chuẩn bị cho việc lưu trữ trong các định dạng văn bản như CSV hoặc JSON.

## Cách thực hiện:
Dưới đây là các ví dụ về cách chuyển đổi một ngày thành chuỗi trong Bash:

```Bash
# Hiển thị ngày và giờ hiện tại theo định dạng mặc định
echo $(date)

# Định dạng tuỳ chỉnh: YYYY-MM-DD
echo $(date '+%Y-%m-%d')

# Bao gồm thời gian
echo $(date '+%Y-%m-%d %H:%M:%S')

# Chuyển đổi một ngày đã tồn tại
existing_date='2023-03-17 08:00:00'
date -d "$existing_date" '+%A, %B %d, %Y'
```
Kết quả mẫu cho các lệnh trên:

```
Sat Mar 25 12:04:22 PDT 2023
2023-03-25
2023-03-25 12:04:22
Thứ Sáu, Ngày 17 Tháng 3, 2023
```

## Sâu hơn
Các hệ thống tương tự Unix đã sử dụng lệnh `date` từ sớm để xử lý ngày và giờ. Sự linh hoạt của nó cho phép sử dụng nhiều định dạng khác nhau, nhờ vào các chỉ thị định dạng như `%Y` cho năm và `%d` cho ngày.

Có các lựa chọn thay thế cho lệnh `date` nếu bạn sử dụng một ngăn xếp công nghệ khác. Chẳng hạn, Python có `datetime.strftime`, trong khi JavaScript cung cấp đối tượng `Date` với các phương thức như `toLocaleDateString()`.

Khi chuyển đổi các ngày trong Bash, hãy nhớ rằng lệnh `date` có thể làm việc với dấu thời gian hiện tại của hệ thống hoặc một ngày đã cung cấp. Xử lý múi giờ cũng rất quan trọng để đảm bảo ngày chính xác.

## Xem thêm
- GNU coreutils 'date': https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- Hướng dẫn lập trình Bash Nâng Cao: https://tldp.org/LDP/abs/html/
- Các chỉ thị định dạng cho lệnh date: https://man7.org/linux/man-pages/man1/date.1.html

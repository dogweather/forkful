---
aliases:
- /vi/fish-shell/downloading-a-web-page/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:40.200969-07:00
description: "T\u1EA3i m\u1ED9t trang web v\u1EC1 m\xE1y \u0111\u1ED3ng ngh\u0129\
  a v\u1EDBi vi\u1EC7c b\u1EA1n k\xE9o n\u1ED9i dung c\u1EE7a trang \u0111\xF3 t\u1EEB\
  \ web v\u1EC1 m\xE1y c\u1EE5c b\u1ED9 c\u1EE7a m\xECnh. L\u1EADp tr\xECnh vi\xEA\
  n th\u1EF1c hi\u1EC7n \u0111i\u1EC1u n\xE0y cho c\xE1c m\u1EE5c\u2026"
lastmod: 2024-02-18 23:08:51.183810
model: gpt-4-0125-preview
summary: "T\u1EA3i m\u1ED9t trang web v\u1EC1 m\xE1y \u0111\u1ED3ng ngh\u0129a v\u1EDB\
  i vi\u1EC7c b\u1EA1n k\xE9o n\u1ED9i dung c\u1EE7a trang \u0111\xF3 t\u1EEB web\
  \ v\u1EC1 m\xE1y c\u1EE5c b\u1ED9 c\u1EE7a m\xECnh. L\u1EADp tr\xECnh vi\xEAn th\u1EF1\
  c hi\u1EC7n \u0111i\u1EC1u n\xE0y cho c\xE1c m\u1EE5c\u2026"
title: "T\u1EA3i trang web"
---

{{< edit_this_page >}}

## Gì và Tại sao?
Tải một trang web về máy đồng nghĩa với việc bạn kéo nội dung của trang đó từ web về máy cục bộ của mình. Lập trình viên thực hiện điều này cho các mục đích tự động hóa, thu thập dữ liệu, truy cập ngoại tuyến hoặc kiểm thử.

## Cách thực hiện:
Dưới đây là cách nhanh và đơn giản để tải một trang web sử dụng Fish Shell với lệnh `curl`:

```fish
curl -O http://example.com/
```

Lệnh này sẽ tải nội dung của trang web và lưu nó với tên giống như tên tập tin trên máy chủ (`index.html` trong hầu hết các trường hợp).

Giờ, nếu bạn muốn lưu nó với một tên khác:

```fish
curl -o trang_cua_toi.html http://example.com/
```

Muốn xem bạn đang tải cái gì? Đây là cách để in nó ra console:

```fish
curl http://example.com/
```

Kết quả mẫu có thể trông như thế này:

```
<!doctype html>
<html>
<head>
    <title>Ví dụ Miền</title>
...
```

## Khám phá sâu hơn
Ngày xưa, việc tải các trang web chủ yếu là một loại "phép thuật" dòng lệnh hơn là bất cứ thứ gì khác. Các công cụ như `wget` và `curl` trở thành những thứ không thể thiếu. `curl`, ra đời từ năm '97, đã chứng minh được thời gian bền bỉ của mình trong việc giao tiếp dữ liệu sử dụng cú pháp URL.

Tại sao chọn `curl` thay vì `wget`? `curl` giống như một công cụ đa năng cho việc truyền dữ liệu, xử lý với nhiều giao thức và các định dạng dữ liệu. Mặc dù cả hai đều có thể tải xuống các trang web, `curl` còn có thể tải lên dữ liệu, và nó hỗ trợ nhiều giao thức hơn và thường được sử dụng làm công cụ phía sau bởi các phần mềm khác.

Chính Fish Shell không tải web pages; nó chỉ là giao diện. Nhưng khi kết hợp nó với `curl`, bạn sẽ có một thiết bị tải web đơn giản mà mạnh mẽ.

Một số người có thể đề cập đến việc sử dụng các công cụ hiện đại hơn như `httpie` hoặc tự động hóa dựa trên trình duyệt với các công cụ như Selenium cho các nhiệm vụ phức tạp hơn như xử lý các trang nặng về Javascript. Tuy nhiên, cho việc tải xuống nhanh chóng và đơn giản, `curl` vẫn chiếm ưu thế.

## Xem Thêm

- Trang web dự án curl để biết thêm chi tiết: [https://curl.se/](https://curl.se/)
- Để khám phá sâu hơn về các thao tác HTTP với `curl`, xem trang hướng dẫn: `man curl`
- httpie là một lựa chọn thay thế cho HTTP client thân thiện với người dùng: [https://httpie.org/](https://httpie.org/)
- Tài liệu Fish Shell cho việc xử lý các công việc liên quan đến shell khác: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)

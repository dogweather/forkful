---
title:                "Gửi một yêu cầu HTTP"
date:                  2024-01-28T22:08:00.575825-07:00
model:                 gpt-4-0125-preview
simple_title:         "Gửi một yêu cầu HTTP"

category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/kotlin/sending-an-http-request.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại sao?

Gửi một yêu cầu HTTP giống như yêu cầu một máy chủ web thực hiện điều gì đó hoặc cung cấp cho bạn thứ gì đó. Các lập trình viên thực hiện việc này để tương tác với các dịch vụ web, truy xuất dữ liệu, gửi các biểu mẫu, hoặc giao tiếp với các API.

## Làm thế nào:

Kotlin làm cho việc gửi yêu cầu HTTP trở nên dễ dàng. Dưới đây là một ví dụ cơ bản sử dụng `khttp`, một thư viện thân thiện với người dùng:

```Kotlin
import khttp.get

fun main() {
    val response = get("https://api.github.com/users/octocat/orgs")
    println(response.text)
}
```

Đầu ra:

```Kotlin
[{"login":"octo-org","id":583231,"url":"https://api.github.com/orgs/octo-org", ...}]
```

Để đáp ứng các nhu cầu phức tạp hơn, đây là một đoạn mã sử dụng `ktor`, một framework của Kotlin, để lấy dữ liệu một cách bất đồng bộ:

```Kotlin
import io.ktor.client.*
import io.ktor.client.engine.cio.*
import io.ktor.client.request.*

suspend fun main() {
    val client = HttpClient(CIO)
    val response: String = client.get("https://api.github.com/users/octocat/orgs")
    println(response)
    client.close()
}
```

Đầu ra tương tự như ví dụ đầu tiên.

## Tìm hiểu sâu

Thư viện `khttp` là một công cụ tiện lợi, mô phỏng theo `requests` của Python. Nó tuyệt vời cho các kịch bản nhanh chóng nhưng không được bảo trì tích cực. `ktor` là một dự án mới hơn, hoạt động của JetBrains, được thiết kế với coroutines cho hoạt động bất đồng bộ. Nó được dùng cho các ứng dụng có khả năng mở rộng. Cả hai đều xử lý yêu cầu HTTP nhưng phục vụ cho các trường hợp sử dụng khác nhau.

Trong quá khứ, các yêu cầu HTTP trong Kotlin được thực hiện với các thư viện Java như `HttpURLConnection` hoặc `HttpClient` của Apache. Những thư viện này vẫn còn hợp lệ nhưng cồng kềnh hơn và thiếu các tính năng ngôn ngữ của Kotlin.

Về việc triển khai, hãy nhớ xử lý các lỗi HTTP thông thường và đọc mã phản hồi. Bạn cũng sẽ muốn sử dụng `try-catch` cho các ngoại lệ mạng và có thể cần phải làm việc với headers và các tham số truy vấn.

## Xem thêm

- Tài liệu Ktor: https://ktor.io/
- Kho GitHub khttp: https://github.com/jkcclemens/khttp (Lưu ý trạng thái bảo trì)
- Gọi HTTP trong Kotlin với HttpURLConnection: https://kotlinlang.org/api/latest/jvm/stdlib/java.net/-http-u-r-l-connection/

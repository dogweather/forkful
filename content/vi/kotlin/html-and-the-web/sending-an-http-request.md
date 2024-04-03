---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:00.575825-07:00
description: "L\xE0m th\u1EBF n\xE0o: Kotlin l\xE0m cho vi\u1EC7c g\u1EEDi y\xEAu\
  \ c\u1EA7u HTTP tr\u1EDF n\xEAn d\u1EC5 d\xE0ng. D\u01B0\u1EDBi \u0111\xE2y l\xE0\
  \ m\u1ED9t v\xED d\u1EE5 c\u01A1 b\u1EA3n s\u1EED d\u1EE5ng `khttp`, m\u1ED9t th\u01B0\
  \ vi\u1EC7n th\xE2n thi\u1EC7n v\u1EDBi ng\u01B0\u1EDDi d\xF9ng."
lastmod: '2024-03-13T22:44:36.597473-06:00'
model: gpt-4-0125-preview
summary: "Kotlin l\xE0m cho vi\u1EC7c g\u1EEDi y\xEAu c\u1EA7u HTTP tr\u1EDF n\xEA\
  n d\u1EC5 d\xE0ng."
title: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP"
weight: 44
---

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

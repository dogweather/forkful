---
title:                "Tải trang web"
date:                  2024-01-28T22:00:05.192144-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tải trang web"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/kotlin/downloading-a-web-page.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái Gì & Tại Sao?
Tải một trang web có nghĩa là lấy mã HTML từ một URL cụ thể để xem hoặc sử dụng một cách nội bộ. Lập trình viên làm điều này cho các công việc như web scraping, đọc offline, hoặc kiểm thử tự động.

## Làm Thế Nào:
Hãy tiến hành với `HttpURLConnection` của Kotlin để nhanh chóng tải một trang web. Chúng ta cũng sẽ sử dụng coroutines cho các thao tác nền mượt mà. Dưới đây là bài học cơ bản:

```kotlin
import java.net.HttpURLConnection
import java.net.URL
import kotlinx.coroutines.*

fun main() = runBlocking {
    val url = "http://example.com"
    val kết quả = withContext(Dispatchers.IO) {
        downloadWebPage(url)
    }
    println(kết quả)
}

fun downloadWebPage(urlAddress: String): String {
    val url = URL(urlAddress)
    val kết nối = url.openConnection() as HttpURLConnection
    try {
        kết nối.connect()
        return kết nối.inputStream.bufferedReader().use { it.readText() }
    } finally {
        kết nối.disconnect()
    }
}
```

Kết quả mẫu:

```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```
Tuyệt vời, phải không? Bạn đã có mã HTML của trang web!

## Tìm Hiểu Sâu Hơn
Việc tải trang web đã xuất hiện từ khi internet ra đời. Trong những năm 90, mọi người sử dụng các công cụ dòng lệnh như `wget` và `curl`. Chúng vẫn còn tồn tại nhưng khi bạn muốn kiểm soát nhiều hơn hoặc tích hợp việc tải nội dung web vào một ứng dụng, bạn sẽ cần lập trình.

Trong Kotlin, bạn có thể sử dụng `HttpURLConnection` của Java hoặc các thư viện như OkHttp hoặc Ktor cho một phương pháp mạnh mẽ với nhiều tính năng hơn. Ví dụ trên là cơ bản; trong thực tế, bạn sẽ cần xem xét đến việc xử lý lỗi, điều hướng lại, và hiệu suất. Có thể thêm vào các lần thử lại hoặc giới hạn thời gian? Và bạn không thể quên việc xử lý các bộ mã hóa ký tự và loại nội dung khác nhau.

Bạn cũng cần quan tâm đến luồng. Chắc chắn ta không muốn làm treo luồng chính khi tải một trang lớn, phải không? Do đó, coroutines - chúng cho phép ứng dụng của bạn vẫn phản hồi, tải trong nền mà không gặp trở ngại gì.

## Xem Thêm
- **OkHttp**: https://square.github.io/okhttp/
- **Ktor Client**: https://ktor.io/docs/client.html
- **Kotlin Coroutines**: https://kotlinlang.org/docs/coroutines-overview.html
- **Java HttpURLConnection**: https://docs.oracle.com/javase/8/docs/api/java/net/HttpURLConnection.html

Đó là điều cần biết—lấy trang, hãy thông minh về nó, và luôn tôn trọng dữ liệu cũng như nguồn của nó. Lập trình vui vẻ!

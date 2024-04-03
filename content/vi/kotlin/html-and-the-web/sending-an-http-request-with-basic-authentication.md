---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:14.522381-07:00
description: "X\xE1c th\u1EF1c c\u01A1 b\u1EA3n \u0111\xEDnh k\xE8m m\u1ED9t t\u1ED5\
  \ h\u1EE3p t\xEAn ng\u01B0\u1EDDi d\xF9ng:m\u1EADt kh\u1EA9u v\xE0o m\u1ED9t y\xEA\
  u c\u1EA7u HTTP. C\xE1c nh\xE0 ph\xE1t tri\u1EC3n s\u1EED d\u1EE5ng n\xF3 nh\u01B0\
  \ m\u1ED9t c\xE1ch nhanh ch\xF3ng v\xE0 b\u1EA9n \u0111\u1EC3 ch\u1EE9ng\u2026"
lastmod: '2024-03-13T22:44:36.601308-06:00'
model: gpt-4-0125-preview
summary: "X\xE1c th\u1EF1c c\u01A1 b\u1EA3n \u0111\xEDnh k\xE8m m\u1ED9t t\u1ED5 h\u1EE3\
  p t\xEAn ng\u01B0\u1EDDi d\xF9ng:m\u1EADt kh\u1EA9u v\xE0o m\u1ED9t y\xEAu c\u1EA7\
  u HTTP."
title: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP v\u1EDBi x\xE1c th\u1EF1c c\u01A1 b\u1EA3\
  n"
weight: 45
---

## Cái gì & Tại sao?

Xác thực cơ bản đính kèm một tổ hợp tên người dùng:mật khẩu vào một yêu cầu HTTP. Các nhà phát triển sử dụng nó như một cách nhanh chóng và bẩn để chứng minh ai đang yêu cầu cái gì trên web.

## Cách thực hiện:

Kotlin xử lý yêu cầu HTTP với các thư viện như `ktor` hoặc `okhttp`. Chúng ta sẽ sử dụng `okhttp` cho bây giờ.

Trước tiên, thêm thư viện vào build.gradle của bạn:

```groovy
dependencies {
    implementation("com.squareup.okhttp3:okhttp:4.9.0")
}
```

Đến lúc viết mã:

```kotlin
import okhttp3.Credentials
import okhttp3.OkHttpClient
import okhttp3.Request
import java.io.IOException

fun main() {
    val client = OkHttpClient()

    val username = "admin"
    val password = "password123"
    val credentials = Credentials.basic(username, password)

    val request = Request.Builder()
        .url("http://example.com/resource")
        .header("Authorization", credentials)
        .build()

    client.newCall(request).execute().use { response ->
        if (!response.isSuccessful) throw IOException("Mã không mong đợi $response")

        println(response.body!!.string())
    }
}
```

Chạy và xem kết quả trên console. Bạn sẽ thấy tài nguyên được bảo vệ hiện ra.

## Nâng cao

Ngày xưa, HTTP Basic Auth là lựa chọn hàng đầu. Đơn giản: chỉ cần mã hóa base64 `username:password` và đặt nó vào header. Không an toàn nếu dùng một mình, do đó HTTPS đã được thêm vào.

Có phương án thay thế không? Rất nhiều. OAuth cho token, API keys cho sự đơn giản, hoặc xác thực digest cho một nâng cấp. Xác thực cơ bản tốt để bắt đầu hoặc cho các công cụ nội bộ, nhưng không phải cho web hiện đại, đề cao an ninh.

Chi tiết thực hiện: Đừng tự chế lại bánh xe. Các thư viện xử lý mã hóa và nắm bắt các sắc thái của giao thức. OkHttp thậm chí còn đối phó với các lần thử lại và kết nối cho bạn. Nhớ rằng, xác thực cơ bản qua HTTP là không nên - luôn sử dụng HTTPS để giữ an toàn cho thông tin xác thực khi chuyển tiếp.

## Xem thêm

- Tài liệu chính thức của OkHttp: [https://square.github.io/okhttp/](https://square.github.io/okhttp/)
- Trang về ngôn ngữ Kotlin (cho mọi thứ về Kotlin): [https://kotlinlang.org/](https://kotlinlang.org/)
- Tìm hiểu thêm về Xác thực Cơ bản: [https://tools.ietf.org/html/rfc7617](https://tools.ietf.org/html/rfc7617)
- Các phương án thay thế cho Xác thực Cơ bản như OAuth 2.0: [https://oauth.net/2/](https://oauth.net/2/)

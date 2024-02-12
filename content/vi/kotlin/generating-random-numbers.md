---
title:                "Sinh số ngẫu nhiên"
aliases:
- vi/kotlin/generating-random-numbers.md
date:                  2024-01-28T22:01:46.496413-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sinh số ngẫu nhiên"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/kotlin/generating-random-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại sao?

Việc tạo ra các số ngẫu nhiên trong lập trình là về việc tạo ra những con số không có bất kỳ một mẫu đoán trước nào. Các lập trình viên thực hiện điều này với nhiều lý do khác nhau, bao gồm mô phỏng, kiểm thử thuật toán, trò chơi, và các ứng dụng bảo mật, nơi sự không dự đoán được là chìa khóa để đạt được kết quả thực tế hoặc an toàn.

## Làm thế nào:

Kotlin cung cấp một cách trực tiếp để tạo ra các số ngẫu nhiên qua thư viện chuẩn của mình. Dưới đây là cách bạn có thể tạo ra các loại giá trị ngẫu nhiên khác nhau:

### Tạo một Số Nguyên Ngẫu Nhiên

Để tạo một số nguyên ngẫu nhiên trong một phạm vi cụ thể:

```kotlin
import kotlin.random.Random

fun main() {
    val randomNumber = Random.nextInt(1, 100) // Tạo một số ngẫu nhiên giữa 1 và 99
    println(randomNumber)
}
```

### Tạo một Số Thực Kép Ngẫu Nhiên

Tương tự, để tạo một số thực kép ngẫu nhiên:

```kotlin
import kotlin.random.Random

fun main() {
    val randomDouble = Random.nextDouble(1.0, 10.0) // Tạo một số thực kép ngẫu nhiên giữa 1.0 và 10.0
    println(randomDouble)
}
```

### Tạo một Giá Trị Boolean Ngẫu Nhiên

Để tạo một giá trị boolean ngẫu nhiên:

```kotlin
import kotlin.random.Random

fun main() {
    val randomBoolean = Random.nextBoolean() // Tạo ngẫu nhiên giá trị true hoặc false
    println(randomBoolean)
}
```

### Gieo Hạt để Có Kết Quả Có Thể Tái Tạo

Trong trường hợp bạn cần một chuỗi các số ngẫu nhiên có thể tái tạo (ví dụ, trong kiểm thử), bạn có thể gieo hạt cho trình sinh số ngẫu nhiên:

```kotlin
import kotlin.random.Random

fun main() {
    val seed = 12345L
    val random = Random(seed)
    val randomNumber = random.nextInt(1, 100)
    println(randomNumber)
}
```

## Thảo Luận Sâu hơn

Cách tiếp cận của thư viện chuẩn Kotlin trong việc tạo ra các số ngẫu nhiên sử dụng `java.util.Random` bên dưới, đảm bảo sự hòa trộn giữa sự tiện dụng và hiệu suất. Tuy nhiên, điều quan trọng cần lưu ý là các phương thức này tạo ra các số ngẫu nhiên giả, tức là các số này trông có vẻ ngẫu nhiên nhưng thực sự được tạo ra nhờ quá trình xác định.

Đối với hầu hết các ứng dụng, sự ngẫu nhiên được cung cấp bởi lớp `Random` của Kotlin là đủ. Tuy nhiên, đối với các ứng dụng nhạy cảm về mặt bảo mật hơn, như mật mã học, nơi chất lượng của sự ngẫu nhiên là quan trọng nhất, người ta nên cân nhắc sử dụng `java.security.SecureRandom` thay thế. SecureRandom được thiết kế riêng cho các hoạt động mật mã, cung cấp chất lượng ngẫu nhiên cao hơn, mặc dù có thể có sự đánh đổi về hiệu suất.

Kotlin không tái sáng tạo lốp xe nhưng cung cấp một API của Kotlin thân thiện qua các cơ chế sinh số ngẫu nhiên của Java, khiến việc sử dụng trong các dự án Kotlin trở nên dễ dàng hơn và gọn gàng hơn. Như mọi khi, khi xử lý vấn đề ngẫu nhiên, lập trình viên nên cân nhắc cẩn thận về trường hợp sử dụng để chọn công cụ phù hợp nhất cho công việc.

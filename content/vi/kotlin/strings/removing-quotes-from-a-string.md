---
title:                "Loại bỏ dấu ngoặc kép khỏi chuỗi"
aliases:
- /vi/kotlin/removing-quotes-from-a-string/
date:                  2024-01-28T22:07:49.487228-07:00
model:                 gpt-4-0125-preview
simple_title:         "Loại bỏ dấu ngoặc kép khỏi chuỗi"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/kotlin/removing-quotes-from-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại sao?

Loại bỏ dấu ngoặc khỏi chuỗi có nghĩa là loại bỏ mọi ký tự dấu ngoặc đơn (' ') hoặc dấu ngoặc kép (" ") khỏi dữ liệu văn bản mà bạn đang làm việc. Các lập trình viên thường cần làm điều này để làm sạch dữ liệu, chuẩn bị cho việc xử lý tiếp theo, hoặc khi chính dấu ngoặc không liên quan đến ý nghĩa của dữ liệu.

## Làm thế nào:

Dưới đây là một cách đơn giản để loại bỏ cả hai loại dấu ngoặc khỏi một chuỗi trong Kotlin:

```kotlin
fun removeQuotes(input: String): String {
    return input.replace("\"", "").replace("'", "")
}

fun main() {
    val stringWithQuotes = "Kotlin \"rocks\" it's 'cool'"
    val stringWithoutQuotes = removeQuotes(stringWithQuotes)
    println(stringWithoutQuotes) // Đầu ra: Kotlin rocks its cool
}
```

Và nếu bạn chỉ muốn loại bỏ một loại dấu ngoặc, chỉ cần bỏ qua lệnh replace khác.

```kotlin
fun removeDoubleQuotes(input: String): String {
    return input.replace("\"", "")
}

fun removeSingleQuotes(input: String): String {
    return input.replace("'", "")
}

fun main() {
    val stringWithQuotes = "Kotlin \"rocks\" it's 'cool'"
    println(removeDoubleQuotes(stringWithQuotes)) // Đầu ra: Kotlin rocks it's 'cool'
    println(removeSingleQuotes(stringWithQuotes)) // Đầu ra: Kotlin "rocks" its cool
}
```

## Sâu hơn

Lịch sử, việc xử lý chuỗi và các ký tự thoát đã là một phần cốt lõi của lập trình, vì văn bản là cách chúng ta giao tiếp với dữ liệu một cách cơ bản. Đôi khi cần phải thoát dấu ngoặc trong chuỗi. Điều này được chỉ định bằng một dấu gạch chéo ngược trước đó (ví dụ, `"She said, \"Hi!\""`). Khi xử lý các chuỗi như vậy, bạn có thể cần loại bỏ các ký tự thoát, hoặc chính dấu ngoặc cho văn bản sạch hơn hoặc dễ sử dụng hơn.

Các phương án thay thế cho phương thức `replace` bao gồm việc loại bỏ dựa trên regex hoặc phân tích cú pháp chuỗi, từng ký tự một. Tuy nhiên, regex có thể quá mức không cần thiết cho các thao tác đơn giản và phân tích cú pháp thủ công kém hiệu quả hơn so với việc sử dụng các hàm chuỗi có sẵn. Hàm `replace` của Kotlin tận dụng phương thức `replace` của `String` trong Java, được tối ưu hóa tốt cho hiệu suất.

Về mặt triển khai, đáng chú ý là Kotlin tương thích với Java, vì vậy, về cơ bản, bất kỳ thao tác nào bạn thực hiện trên chuỗi đều hiệu quả như khi bạn thực hiện trong Java. Khi loại bỏ dấu ngoặc, điều quan trọng là phải nhận thức được các trường hợp ngoại lệ, như dấu ngoặc lồng nhau, có thể yêu cầu một cách tiếp cận phức tạp hơn, có khả năng sử dụng biểu thức chính quy hoặc một thư viện phân tích cú pháp.

## Xem thêm

Để biết thêm ngữ cảnh về việc xử lý chuỗi trong Kotlin, bạn có thể tham khảo tài liệu chính thức:

- [Tài liệu về String của Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)

Để tìm hiểu sâu hơn về biểu thức chính quy và phân tích cú pháp trong Kotlin:

- [Tài liệu Regex của Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)

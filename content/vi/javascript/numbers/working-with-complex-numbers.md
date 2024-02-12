---
title:                "Làm việc với số phức"
aliases: - /vi/javascript/working-with-complex-numbers.md
date:                  2024-01-28T22:12:27.522819-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với số phức"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/javascript/working-with-complex-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Số phức là những số có một phần thực và một phần ảo (như 3 + 4i). Chúng xuất hiện trong nhiều vấn đề lập trình, đặc biệt là trong xử lý tín hiệu, tính toán lượng tử, và giải các phương trình đa thức. Các lập trình viên xử lý chúng để hiệu quả hóa việc giải quyết các loại nhiệm vụ này.

## Làm thế nào:
JavaScript không hỗ trợ sẵn số phức, nhưng bạn có thể kỳ công và xử lý nó bằng cách sử dụng đối tượng và toán học. Dưới đây là một cái nhìn nhanh.

```javascript
class SốPhức {
  constructor(thực, ảo) {
    this.thực = thực;
    this.ảo = ảo;
  }

  cộng(khác) {
    return new SốPhức(this.thực + khác.thực, this.ảo + khác.ảo);
  }

  // ...thêm các phương thức khác (trừ, nhân, chia) theo nhu cầu

  toString() {
    return `${this.thực} + ${this.ảo}i`;
  }
}

const a = new SốPhức(1, 2);
const b = new SốPhức(3, 4);
const kết_quả = a.cộng(b);

console.log(`Kết quả: ${kết_quả}`); // Kết quả: 4 + 6i
```

## Sâu hơn nữa
Số phức đã tồn tại từ thế kỷ 16, nhờ nhà toán học người Ý Gerolamo Cardano. Chúng trở nên thiết yếu trong nhiều lĩnh vực, như kỹ thuật và vật lý. Trong lập trình hiện đại, chúng là chìa khóa cho các mô phỏng và thuật toán cần đa chiều.

Bây giờ, JavaScript không hỗ trợ sẵn số phức một cách tự nhiên. Nhưng ngoài phương án tự làm, bạn cũng có thể sử dụng các thư viện toán học như math.js hay numeric.js. Chúng cung cấp sức mạnh cho các nhiệm vụ xử lý số phức nặng hơn, thêm vào đó là các ưu điểm như nhiều thao tác hơn, tính toán độ lớn, và tìm đối số.

Phía bên dưới cơ cấu, khi bạn thao tác với số phức, giống như quản lý hai số riêng biệt gắn kết với nhau. Phép cộng và trừ rất đơn giản—ghép phần thực với thực, ảo với ảo. Phép nhân và chia trở nên phức tạp hơn với những hoạt động chéo và cần sự chăm sóc kỹ lưỡng hơn.

## Xem thêm
- Tài liệu Web MDN về JavaScript: https://developer.mozilla.org/en-US/docs/Web/JavaScript/A_re-introduction_to_JavaScript
- Math.js, một thư viện toán học bao gồm số phức: https://mathjs.org/docs/datatypes/complex_numbers.html
- Numeric.js, một thư viện khác: http://numericjs.com/documentation.html
- Sâu hơn về số phức (tập trung vào toán học): https://mathworld.wolfram.com/ComplexNumber.html

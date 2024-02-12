---
title:                "Làm tròn số"
aliases: - /vi/javascript/rounding-numbers.md
date:                  2024-01-28T22:06:52.640869-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm tròn số"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/javascript/rounding-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Định Nghĩa & Lý Do?
Làm tròn là việc cắt bỏ phần thừa sau một điểm nhất định trong số. Lập trình viên làm tròn để kiểm soát độ chính xác, quản lý bộ nhớ, hoặc tạo ra đầu ra thân thiện với người dùng—như biến 2.998 thành một con số sạch đẹp là 3.

## Cách thực hiện:
Dưới đây là cách bạn làm tròn số trong JavaScript sử dụng `Math.round()`, `Math.ceil()`, và `Math.floor()`: 

```javascript
let originalNumber = 2.567;

let roundedDown = Math.floor(originalNumber); // 2
let roundedUp = Math.ceil(originalNumber);    // 3
let rounded = Math.round(originalNumber);     // 3 (vì .567 lớn hơn .5)

console.log(roundedDown); // In ra: 2
console.log(roundedUp);   // In ra: 3
console.log(rounded);     // In ra: 3
```

Để cố định một số chữ số thập phân nhất định, sử dụng `toFixed()`:

```javascript
let twoDecimals = originalNumber.toFixed(2); // "2.57" (trả về dạng chuỗi)

console.log(twoDecimals); // In ra: "2.57"
```

Chuyển chuỗi trở lại thành số bằng cách sử dụng dấu cộng đơn hoặc `Number()`:

```javascript
let numberAgain = +twoDecimals; // 2.57

console.log(numberAgain); // In ra: 2.57
```

## Sâu hơn nữa
Làm tròn số không phải là điều mới; nó cùng tuổi với số học. Trong JavaScript, `Math.round()` sử dụng phương pháp "làm tròn một nửa lên": nếu phần thập phân là 0.5, nó sẽ làm tròn lên số chẵn gần nhất.

Để kiểm soát nhiều hơn, có thể bạn sẽ ưu tiên sử dụng `toFixed()`, nhưng nhớ rằng, nó trả về một chuỗi. Chuyển đổi lại thành số có thể là một bước thêm, nhưng đảm bảo bạn tiếp tục làm việc với kiểu số.

Có lựa chọn khác không? Thư viện như `lodash` cung cấp `_.round(number, [precision=0])` để kiểm soát tinh tế hơn. Hoặc, `Intl.NumberFormat` mới hơn mang lại cho bạn định dạng chính xác cao hơn ngoài việc chỉ làm tròn.

Nói về độ chính xác, hãy cảnh giác với hiện tượng kỳ lạ của số dấu phẩy động trong JavaScript. `0.1 + 0.2` không chính xác bằng `0.3` do cách số được lưu trữ. Đôi khi, việc làm tròn trở nên cần thiết để sửa chữa những lỗi số dấu phẩy động như vậy.

## Xem thêm
- Tài liệu về Math của Mozilla: [MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math)
- Làm tròn tài chính với `Intl.NumberFormat`: [API Quốc tế hóa ECMAScript](https://tc39.es/ecma402/#numberformat-objects)
- Làm tròn của `lodash`: [Tài liệu Lodash](https://lodash.com/docs/4.17.15#round)

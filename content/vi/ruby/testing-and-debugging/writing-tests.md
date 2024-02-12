---
title:                "Viết các bài kiểm tra"
aliases:
- /vi/ruby/writing-tests.md
date:                  2024-01-28T22:13:25.384906-07:00
model:                 gpt-4-0125-preview
simple_title:         "Viết các bài kiểm tra"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/ruby/writing-tests.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái Gì & Tại Sao?

Viết các bài test kiểm tra xem code có hoạt động như mong đợi. Lập trình viên thực hiện việc này để phát hiện lỗi sớm, bảo đảm độ tin cậy và dễ dàng thay đổi code trong tương lai.

## Làm Thế Nào:

Ruby sử dụng Minitest và RSpec cho việc test—hãy sử dụng RSpec. Đầu tiên, cài đặt nó:

```ruby
gem install rspec
```

Tạo một tệp test, `calculator_spec.rb`:

```ruby
RSpec.describe Calculator do
  describe "#add" do
    it "tính tổng hai số" do
      expect(Calculator.new.add(3, 7)).to eql(10)
    end
  end
end
```

Chạy test với:

```shell
rspec calculator_spec.rb
```

Kết quả:

```
F

Các Lỗi:

  1) Calculator#add tính tổng hai số
     Lỗi/Gặp Phải: expect(Calculator.new.add(3, 7)).to eql(10)
     
     NameError:
       hằng số chưa khởi tạo Calculator
```

Tạo `calculator.rb`:

```ruby
class Calculator
  def add(a, b)
    a + b
  end
end
```

Chạy lại các bài test.

Kết quả:

```
.

Hoàn thành trong 0.002 giây (tệp mất 0.08 giây để tải)
1 ví dụ, 0 lỗi
```

## Đào Sâu

Việc test trong Ruby quay trở lại với Test::Unit, nhưng RSpec, giới thiệu năm 2005, đã cách mạng hóa việc test Ruby với "phát triển dựa trên hành vi". Các lựa chọn thay thế cho RSpec bao gồm Minitest và Test::Unit. RSpec tập trung vào khả năng đọc và mặt doanh nghiệp; Minitest minimalist hơn và nhanh hơn. Thông thường, các bài test mô phỏng cách sử dụng phần mềm, kiểm tra các chức năng, dữ liệu và trường hợp rìa. Đối với các dự án đã tồn tại, bắt đầu bằng cách test những phần quan trọng nhất.

## Xem Thêm

- RSpec GitHub: [github.com/rspec/rspec](https://github.com/rspec/rspec)
- Minitest: [rubygems.org/gems/minitest](https://rubygems.org/gems/minitest)
- "Effective Testing with RSpec 3": Đọc thêm về nguyên tắc và mô hình RSpec.

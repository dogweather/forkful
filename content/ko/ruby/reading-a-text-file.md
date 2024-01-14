---
title:    "Ruby: 텍스트 파일 읽기"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

# 왜: 텍스트 파일을 읽는 방법

텍스트 파일을 읽는 것은 프로그래밍에서 중요한 기술 중 하나입니다. 텍스트 파일을 읽는 것을 배우면 다양한 형식의 데이터를 쉽게 다룰 수 있고, 더 효과적인 프로그램을 작성할 수 있습니다.

## 어떻게: 텍스트 파일 읽기 예제

텍스트 파일을 읽는 것은 간단한 작업입니다. 다음 예제 코드를 따라하며 텍스트 파일을 읽는 방법을 배워보세요.

```ruby
# 파일을 읽어오기 위해 File.open() 메소드를 사용합니다.
file = File.open("example.txt", "r")

# 각 라인을 순차적으로 읽기 위해 each_line() 메소드를 사용합니다.
file.each_line do |line|
  # 각 라인의 내용을 출력합니다.
  puts line
end

# 파일을 닫아줍니다.
file.close
```

위 코드를 실행하면 "example.txt" 파일 내용이 한 줄씩 출력될 것입니다.

```
Hello, world!
This is a sample text file.
```

## 깊게 들어가기: 텍스트 파일의 내부 구조

텍스트 파일은 문자 인코딩 방식에 따라 다양한 형식으로 저장될 수 있습니다. 대표적인 인코딩 방식으로는 UTF-8, ASCII, EUC-KR 등이 있습니다.

또한, 텍스트 파일은 기본적으로 텍스트 형식의 데이터만을 저장할 수 있기 때문에 이미지나 동영상 같은 바이너리 데이터는 저장할 수 없습니다.

## 참고자료

- [Ruby 공식 문서 - 파일 입력](https://ruby-doc.org/core-2.6.3/File.html#method-c-read)
- [Do it! 점프 투 루비](http://www.yes24.com/Product/Goods/7149756)
- [hello_comment.tistory.com](http://hello-comment.tistory.com/entry/%EB%A6%AC%EB%B7%B0-%EB%8F%84%EC%BB%A4-%ED%8E%98%EC%9D%B4%EC%A7%80-%ED%8C%8C%EC%9D%BC-%EC%9D%BD%EA%B8%B0-%EC%98%88%EC%A0%9C%EB%AC%B8%EC%84%9C)
- [제타위키 - 파일 입출력](https://zetawiki.com/wiki/%EB%8B%A4%EB%A5%B8%EB%B0%A9_%ED%8C%8C%EC%9D%BC_%EC%9D%BD%EA%B8%B0)
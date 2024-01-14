---
title:    "Ruby: 텍스트 파일 읽기"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

누군가가 텍스트 파일을 읽는 것에 참여하는 이유는 다양합니다. 예를 들어, 텍스트 파일은 데이터를 저장하고 공유하는 데 사용되며, 프로그래밍 시험에서 요구 사항을 이해하는 데 도움이 될 수 있습니다.

## 어떻게

텍스트 파일을 읽는 것은 간단하게 보일 수 있지만, 우리는 Ruby 언어를 사용하여 실제로 어떻게 할 수 있는지 살펴보겠습니다. 아래 코드 블록에서, 우리는 텍스트 파일을 여는 방법과 해당 파일에서 데이터를 읽는 방법을 보여줍니다.

```Ruby
# 텍스트 파일 열기
file = File.open("sample.txt")

# 파일에서 한 줄씩 읽기
file.each do |line|
    puts line
end
```

위 코드를 실행하면 "sample.txt" 파일의 모든 내용이 한 줄씩 출력됩니다. 반복문을 통해 파일의 모든 줄을 읽을 수도 있고, `readlines` 메서드를 사용하여 파일의 모든 줄을 배열로 저장할 수도 있습니다.

또한 다양한 옵션을 사용하여 파일을 읽을 수 있습니다. 예를 들어, `read` 메서드를 사용하면 파일의 모든 내용을 하나의 문자열로 읽을 수 있습니다.

```Ruby
# 파일 전체 내용 읽기
contents = file.read
puts contents
```

## 깊이 파헤치기

텍스트 파일을 읽는 데 사용되는 Ruby의 `File` 클래스에는 다양한 메서드와 옵션이 있습니다. 예를 들어, 읽기 모드 외에도 쓰기 모드, 추가 모드 등의 다양한 옵션을 설정할 수 있습니다.

또한 파일을 읽는 데 있어서 오류 처리와 관련된 중요한 부분도 있습니다. 파일이 존재하지 않거나 읽을 수 없는 경우 발생할 수 있는 예외 상황을 미리 처리하는 것이 중요합니다.

텍스트 파일을 읽는 것은 여러 이유로 유용하고 중요합니다. 따라서 Ruby를 사용하여 텍스트 파일을 읽는 것을 잘 익히는 것이 중요합니다.

## 참고 자료

- [Ruby 공식 문서 - File 클래스](https://docs.ruby-lang.org/en/2.6.0/File.html)
- [TechNet Wiki - Ruby에서 텍스트 파일 읽기](https://social.technet.microsoft.com/wiki/contents/articles/33819.ruby-read-text-files.aspx)
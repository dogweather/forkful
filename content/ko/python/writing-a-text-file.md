---
title:                "텍스트 파일 작성하기"
html_title:           "Python: 텍스트 파일 작성하기"
simple_title:         "텍스트 파일 작성하기"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

#무엇 & 왜?

텍스트 파일을 작성한다는 것은 글자와 숫자 등의 데이터를 컴퓨터에 저장하는 일이다. 프로그래머들은 이 작업을 하는 이유는 자신이 만든 프로그램에서 필요한 정보를 저장하고 관리하기 위해서이다.

#어떻게:

```python
#새 텍스트 파일 만들기
file = open('example.txt', 'w')

#텍스트 쓰기
file.write('Hello world!')

#파일 닫기
file.close()
```

```python
#존재하는 텍스트 파일 열기
file = open('example.txt', 'r')

#파일에서 텍스트 읽기
text = file.read()

#텍스트 출력
print(text)
```

```
#출력:
Hello world!
```

#딥 다이브:

- 텍스트 파일은 컴퓨터의 기본적인 데이터 저장 방식 중 하나이다.
- 다른 대안으로는 데이터베이스나 스프레드시트 등을 이용할 수 있다.
- 텍스트 파일을 작성하는 방법은 간단하지만 최근에는 데이터베이스나 스프레드시트가 더 널리 사용되고 있다.

#참고:
- [Python 파일 다루기](https://dojang.io/mod/page/view.php?id=2333)
- [파일 입출력 - open()과 close()](https://wikidocs.net/26)
- [파일 처리 방식 비교](https://blog.naver.com/lm3357/221408662194)
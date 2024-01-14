---
title:    "Bash: 텍스트 파일 읽기"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

왜 텍스트 파일을 읽는 것이 중요할까요? 텍스트 파일은 우리가 일상적으로 사용하는 많은 소프트웨어에서 중요한 역할을 합니다. 예를 들어, 당신의 컴퓨터에 저장된 문서나 이메일은 모두 텍스트 파일로 이루어져 있습니다. 따라서 텍스트 파일을 이해하고 읽는 것은 중요한 프로그래밍 기술입니다.

## 어떻게

```Bash
# 새 파일 생성
touch sample.txt

# 파일에 내용 추가하기
echo "안녕하세요, 하루입니다!" >> sample.txt

# 파일 읽기
cat sample.txt
```
출력:
```
안녕하세요, 하루입니다!
```

이 예제에서 우리는 우선 sample.txt라는 새 파일을 생성하고, 그 파일에 "안녕하세요, 하루입니다!"라는 문구를 추가하였습니다. 그리고 마지막으로 cat 명령어를 이용하여 파일의 내용을 화면에 출력하는 것을 볼 수 있습니다.

## 딥 다이브

텍스트 파일을 읽는 방법은 다양하지만 가장 기본적인 방법은 cat 명령어를 사용하는 것입니다. 하지만 이 외에도 다른 유용한 도구들이 있습니다. 예를 들어, grep 명령어를 사용하면 파일에서 특정 단어나 문구를 찾을 수 있습니다. 또한, awk와 sed와 같은 명령어를 이용하여 파일의 내용을 가공하고 원하는 형태로 출력할 수 있습니다.

## 참고자료

* [Bash 프로그래밍 기초](http://codetorial.net/bash/index.html)
* [grep 명령어 사용법](https://www.hypertextstories.co.kr/%EA%B9%83%EB%9D%BC%EC%A0%80-%EA%B8%B0%EC%B6%9C/%EC%8B%A4%ED%96%A5-%EB%93%A4%EC%96%B4%EA%B0%80%EA%B8%B0/6-%EA%B7%B8%EB%A6%AC%ED%94%84-%EB%AA%85%EB%A0%B9%EC%96%B4%EA%B0%80-%EA%B0%80%EC%9E%A5-%EC%99%84%EB%A3%AC%ED%95%98%EA%B2%8C-%EB%B3%B4%EB%82%98%EC%9A%94)
* [awk 명령어 사용법](https://zetawiki.com/wiki/Bash_%EC%9C%A0%EC%82%AC%EC%9A%B0_awk)
* [sed 명령어 사용법](https://m.blog.naver.com/PostView.nhn?blogId=aiins/personal/20100151630)
* [GNU/Linux 명령어 정리](https://brownbears.tistory.com/151)
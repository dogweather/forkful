---
title:                "미래나 과거의 날짜 계산하기"
html_title:           "Bash: 미래나 과거의 날짜 계산하기"
simple_title:         "미래나 과거의 날짜 계산하기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜

날짜를 미래 또는 과거로 계산하는 것의 이유는 우리가 일상 생활에서 자주 필요한 작업입니다. 예를 들어, 어떤 날짜에 대한 예약을 하거나, 과거 날짜의 기록을 찾고 싶을 때 등 다양한 상황에서 날짜 계산은 필수적인 기능입니다.

## 하는 법

우리는 Bash의 내장 명령어인 `date`를 사용하여 미래 또는 과거 날짜를 계산할 수 있습니다. 이를 위해 다음과 같은 형식을 사용합니다.

```
date -d "MM/DD/YYYY + n days/weeks/months/years"
```

여기서 MM/DD/YYYY는 기준이 되는 날짜이며, n은 더하거나 빼고자 하는 날짜의 수입니다. 예를 들어, 내일의 날짜를 계산하려면 다음과 같이 입력하면 됩니다.

```
date -d "06/22/2021 + 1 day"
```

출력은 "Tue Jun 22 00:00:00 KST 2021"과 같은 형식으로 나타납니다.

계산된 날짜를 년도, 월, 일과 같이 보다 자세한 형식으로 출력하려면 `-I` 옵션을 사용할 수 있습니다. 예를 들어, 위의 예제를 아래와 같이 입력하면 됩니다.

```
date -I -d "06/22/2021 + 1 day"
```

그러면 "2021-06-22"라는 형식으로 출력됩니다.

## 깊게 파헤치기

`date` 명령어는 다양한 옵션을 가지고 있어 매우 유용합니다. 예를 들어, 다음과 같은 옵션들이 있습니다.

- `-I` : 출력 형식을 년, 월, 일 형식으로 지정합니다.
- `-R` : 출력 형식을 RFC2822 형식으로 지정합니다.
- `-u` : 출력 시간을 UTC로 지정합니다.
- `-r` : 입력 파일의 수정 시간을 사용하여 날짜를 계산합니다.

또한, `--date`나 `-d` 옵션으로 입력된 날짜 형식은 다양한 형태로 지정할 수 있습니다. 자세한 사항은 `man date`를 통해 확인할 수 있습니다.

## 참고

- [Bash 공식 문서 - date](https://www.gnu.org/software/bash/manual/bash.html#Date-Input-Formats)
- [Linuxize - How to Use the date Command in Linux](https://linuxize.com/post/how-to-use-date-command-in-linux/)

[//]: # (This is a comment and it won't be included in the output)
[//]: # (I am using it to show the output in the preview section)
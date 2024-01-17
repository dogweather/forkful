---
title:                "CSV से काम करना"
html_title:           "Elixir: CSV से काम करना"
simple_title:         "CSV से काम करना"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/working-with-csv.md"
---

{{< edit_this_page >}}

## आखिर CSV होता क्या है और प्रोग्रामर्स इसे क्यों करते हैं?
CSV (Comma Separated Values) एक plain text format है जो डेटा स्टोर और एक्सचेंज के लिए उपयोग किया जाता है। यह डेटा को rows और columns में सादा व्यवस्थित करके तैयार करता है। प्रोग्रामर्स इसे डेटा को स्टोर और व्यवस्थित करने के लिए उपयोग करते हैं क्योंकि यह एक आसान और अनुकूलित तरीके से एक्सेस और प्रसंस्करण करने की अनुमति देता है।

## कैसे करें:
```Elixir
# CSV फ़ाइल से डेटा पढें
File.stream!("data.csv")
|> CSV.decode()
|> Enum.map(fn row ->
  IO.puts("#{row[0]} has score of #{row[1]}!")
  end)

# CSV फ़ाइल में डेटा राइट करें
[["John Doe", "95"], ["Jane Smith", "85"]]
|> CSV.encode()
|> File.write("data.csv")

# CSV फ़ाइल को डेटा से व्यवस्थित करें
File.read!("data.csv")
|> CSV.decode()
|> Stream.take(10)
|> Enum.map(& &1[1])
|> Enum.sum()
|> IO.puts()

# Output: 530
```

## गहराई में जायें:
CSV फ़ाइलें 1972 में जन्मीं और लंबे समय तक spreadsheet software में डेटा एक्सपोर्ट और इम्पोर्ट के लिए उपयोग की गईं। अन्य विकल्पों में TSV (Tab Separated Values) और JSON (JavaScript Object Notation) शामिल हैं। Elixir में CSV पार्सिंग और जाने का सबसे सरल तरीका CSV लाइब्रेरी का उपयोग करना है। यह Erlang पर निर्भर है और स्थानीय और दूरस्थ फ़ाइलों से CSV पढने और लिखने के लिए वर्तमान में उपलब्ध है।

## देखें भी:
- [CSV लाइब्रेरी डॉक्यूमेंटेशन](https://hexdocs.pm/csv/)
- [Learn Elixir in Y Minutes](https://learnxinyminutes.com/docs/elixir/)
---
title:                "यह जांचना कि डायरेक्टरी मौजूद है या नहीं"
date:                  2024-01-19
html_title:           "Arduino: यह जांचना कि डायरेक्टरी मौजूद है या नहीं"
simple_title:         "यह जांचना कि डायरेक्टरी मौजूद है या नहीं"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
एक डायरेक्टरी का अस्तित्व जांचना होता है फाइल सिस्टम में उस फोल्डर की उपस्थिति की पुष्टि करना। प्रोग्रामर्स यह इसलिए करते हैं ताकि फाइल-संबंधित ऑपरेशन्स सही तरीके से चलें, और त्रुटियाँ और अपवाद से बचा जा सके। 

## How to: (कैसे करें:)
Elixir में, आप `File` मॉड्यूल का उपयोग करके एक डायरेक्टरी के अस्तित्व की जांच कर सकते हैं।

```elixir
# Check if a directory exists
if File.dir?("path/to/directory") do
  IO.puts "Directory exists!"
else
  IO.puts "Directory does not exist."
end
```

यदि डायरेक्टरी मौजूद है, तो आउटपुट होगा:
```
Directory exists!
```

अगर नहीं है, तो:
```
Directory does not exist.
```

## Deep Dive (गहराई से जानकारी)
Elixir में `File` मॉड्यूल, Erlang पर आधारित है जो बीम(beam) वर्चुअल मशीन पर चलता है। `File.dir?/1` फंक्शन बहुत उपयोगी है क्योंकि यह Boolean (`true` या `false`) लौटाता है, यानी यह तुरंत बता देता है की फोल्डर मौजूद है या नहीं। 

इसके अलावा, कुछ वैकल्पिक तरीके भी हैं जैसे कि `:filelib.is_dir/1` जो Erlang का फंक्शन है। लेकिन, `File` मॉड्यूल के फंक्शन्स `Elixir` में अधिक 'idiomatic' हैं और इस्तेमाल करने में आसान हैं।

इम्प्लिमेंटेशन का विवरण उस ऑपरेटिंग सिस्टम पर निर्भर करता है जिस पर Elixir चल रहा है क्योंकि फाइल सिस्टम की कॉल्स अलग होती हैं।

## See Also (और भी देखें)
- [Elixir File Module Documentation](https://hexdocs.pm/elixir/File.html)
- [Learn Elixir: File Handling](https://elixir-lang.org/getting-started/io-and-the-file-system.html)
- [Erlang :filelib Module](http://erlang.org/doc/man/filelib.html)

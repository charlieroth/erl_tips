☎️ Erlang Tip #16 - Skip GenServer Start ☎️

Did you know that you can prevent a GenServer from starting by retuning :ignore in your init/1 callback? This is useful if you want to control whether a GenServer starts at run-time.

https://twitter.com/akoutmos/status/1701279443291881568
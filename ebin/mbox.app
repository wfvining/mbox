{application,
 mbox,
 [{vsn, "1.0.0"},
  {modules, [mbox, mbox_sup, mbox_serv, mbox_box_sup]},
  {registered, [mbox]},
  {mod, {mbox, []}},
  {env, [{max_size, 1000}]}]
}.
